{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE DerivingVia #-}

module Hydra.Events.AWS.Kinesis where

import Hydra.Prelude

import Amazonka (Base64 (Base64), Env)
import Amazonka qualified as AWS
import Amazonka.Kinesis qualified as AWS
import Amazonka.Kinesis.GetRecords qualified as AWS
import Amazonka.Kinesis.Lens
import Amazonka.Kinesis.PutRecord qualified as AWS
import Control.Lens (lazy, each, _Just)
import Control.Lens.Operators ((^.), (^..), (^?), (.~))
import Data.Aeson (encode)
import Data.Aeson.Decoding (eitherDecode)
import Hydra.Events (EventSink (..), EventSource (..), HasEventId (..))
import System.IO qualified as IO
import Control.Monad.Except

import Hydra.Options (KinesisConfig (..))
import Control.Monad.Fix
import Control.Lens.Combinators (set, view)
import qualified Amazonka.Kinesis.Lens as AWS
import Amazonka.Kinesis (ShardFilterType(ShardFilterType_FROM_TRIM_HORIZON))
import Control.Monad.Class.MonadTime (MonadMonotonicTimeNSec(getMonotonicTimeNSec))
import qualified Control.Monad.Class.MonadTimer as MT
import Data.Time (secondsToDiffTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)

-- | An exception that can be thrown by the Kinesis sink or source
data KinesisException = KinesisUnmarshalException String | KinesisAWSError String AWS.Error
  deriving stock (Show)
  deriving anyclass (Exception)

newtype Kinesis a = Kinesis { unKinesis :: ReaderT KinesisEnv (ExceptT KinesisException IO) a }
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadThrow, MonadError KinesisException, MonadReader KinesisEnv, MonadFix)
-- can replace instances for debugging, stack trace, etc

instance MonadMonotonicTimeNSec Kinesis where
  getMonotonicTimeNSec = liftIO getMonotonicTimeNSec

instance MonadMonotonicTime Kinesis where
  getMonotonicTime = liftIO (getMonotonicTime @IO)

instance MT.MonadDelay Kinesis where
  threadDelay = liftIO . MT.threadDelay

instance MonadDelay Kinesis where
  threadDelay = liftIO . threadDelay

type KinesisEnv = (AWS.Env, KinesisConfig)

runKinesis :: Env -> KinesisConfig -> Kinesis a -> IO (Either KinesisException a)
runKinesis awsEnv awsConfig action = runExceptT $ runReaderT (unKinesis action) (awsEnv, awsConfig)

runKinesisThrowIO :: Env -> KinesisConfig -> Kinesis b -> IO b
runKinesisThrowIO awsEnv awsConfig action = either throwIO pure =<< runKinesis awsEnv awsConfig action

sendThrow :: (MonadError KinesisException m, MonadIO m, AWS.AWSRequest a,  Typeable a, Typeable (AWS.AWSResponse a)) => String -> Env -> a -> m (AWS.AWSResponse a)
sendThrow task env sendRecord = do
  runResourceThrowKinesisError $ AWS.sendEither env sendRecord
 where
  runResourceThrowKinesisError = wrapErrorKinesis <=< runResourceKinesis . fmap hoistEither
  wrapErrorKinesis = modifyError (KinesisAWSError $ "AWS Exception during " <> task)
  runResourceKinesis = liftIO . AWS.runResourceT

exampleKinesisEventPair :: forall e. (ToJSON e, FromJSON e) => KinesisConfig -> IO (EventSource e IO, EventSink e IO)
exampleKinesisEventPair kinesisConfig@KinesisConfig{kinesisSinkEnabled, kinesisSourceEnabled} = do
  awsLogger <- AWS.newLogger AWS.Debug IO.stdout -- TODO(Elaine): we can use our own nice logging
  awsDiscoveredEnv <- AWS.newEnv AWS.discover
  let awsEnv = awsDiscoveredEnv{AWS.logger = awsLogger}

  source <- if kinesisSourceEnabled then runKinesisThrowIO awsEnv kinesisConfig exampleKinesisSource else pure (EventSource $ pure [])
  sink <- if kinesisSinkEnabled then runKinesisThrowIO awsEnv kinesisConfig exampleKinesisSink else pure (EventSink $ void . pure)


  pure
    ( EventSource $ runKinesisThrowIO awsEnv kinesisConfig (getEvents source)
    , EventSink $ runKinesisThrowIO awsEnv kinesisConfig . putEvent sink
    )

exampleKinesisSink :: ToJSON e => Kinesis (EventSink e Kinesis)
exampleKinesisSink = do
  (awsEnv, KinesisConfig{kinesisStreamName, kinesisStreamARN}) <- ask

  pure $ EventSink \e -> do
    --TODO(Elaine): this uses multiple shards instead of one
    -- do we want this?
    let putRecordReq =
          AWS.PutRecord'
            { explicitHashKey = Just . show $ getEventId e
            , sequenceNumberForOrdering = Nothing
            , streamARN = Just kinesisStreamARN
            , streamName = Just kinesisStreamName
            , data' = Base64 . toStrict $ encode e -- TODO(Elaine): is this just a constructor that represents a base64 encoded string?
            , partitionKey = show $ getEventId e
            }
    liftIO . putStrLn $ "Sending event " <> show (getEventId e) <> " to Kinesis"

    response <- sendThrow "Kinesis EventSink Put" awsEnv putRecordReq

    liftIO . putStrLn $ "Sent event " <> show (getEventId e) <> " to Kinesis, with sequence number: " <> show (response ^. AWS.putRecordResponse_sequenceNumber)

exampleKinesisSource :: FromJSON e => Kinesis (EventSource e Kinesis)
exampleKinesisSource = do
  (awsEnv, KinesisConfig{kinesisStreamARN, kinesisStreamName, kinesisResumeTimeStamp, kinesisResumeShardId}) <- ask

  --get shards, assume they wont change while running
  let listShardsReq = AWS.newListShards
        & AWS.listShards_streamARN .~ Just kinesisStreamARN
        & AWS.listShards_streamName .~ Just kinesisStreamName

  let goShards = \case
        Nothing -> pure []
        Just page -> do
          response <- sendThrow "Fetching shards list for Kinesis Event Source" awsEnv page
          let nextPage = AWS.page page response
          rec futureShards <- goShards nextPage
          pure $ (response ^.. listShardsResponse_shards . _Just . each ) <> futureShards

  shards <- goShards (Just listShardsReq)

  --TODO(Elaine): rethrow from Nothing

  let shardIteratorType = fromMaybe AWS.ShardIteratorType_TRIM_HORIZON . asum $
        [ kinesisResumeShardId $> AWS.ShardIteratorType_AT_SEQUENCE_NUMBER
        , kinesisResumeTimeStamp $> AWS.ShardIteratorType_AT_TIMESTAMP ]
  let shardIderatorReq = AWS.newGetShardIterator (maybe "" (view shard_shardId) (viaNonEmpty head shards)) shardIteratorType
        & AWS.getShardIterator_streamName .~ Just kinesisStreamName
        & AWS.getShardIterator_streamARN .~ Just kinesisStreamARN
        & AWS.getShardIterator_timestamp .~ fmap posixSecondsToUTCTime kinesisResumeTimeStamp
  --TODO(Elaine): rethrow from Nothing
  shardIteratorResponse <- sendThrow "Fetching shard iterator for Kinesis Event Source" awsEnv shardIderatorReq <&> fromMaybe "" . view AWS.getShardIteratorResponse_shardIterator

  pure $ EventSource do
    let getRecordsReq =
          AWS.GetRecords'
            { limit = Just 10000 -- TODO(Elaine): we definitely need to tune a particular limit
            -- , shardIterator = maybe "" (view shard_shardId) (viaNonEmpty head shards)
            , shardIterator = shardIteratorResponse
            , streamARN = Just kinesisStreamARN
            }
    putStrLn "Getting records from Kinesis"
    let go Nothing = pure []
        go (Just page) = do
            liftIO . putStrLn $ "Getting events from Kinesis"
            response <- sendThrow "Kinesis EventSource Get" awsEnv page

            loadedEvents <- parseKinesisRecords response
            putStrLn $ "Got " <> show (length loadedEvents) <> " records from Kinesis"
            putStrLn $ "EventIDs obtained: " <> show (fmap getEventId loadedEvents)
            let nextShardIterator = response ^. AWS.getRecordsResponse_nextShardIterator

            threadDelay (secondsToDiffTime 10)

            rec futureLoadedEvents <- do
                  liftIO . putStrLn $ "Getting events from next shard iterator: " <> show nextShardIterator
                  go (set getRecords_shardIterator <$> nextShardIterator <*> pure getRecordsReq)

            pure $ loadedEvents <> futureLoadedEvents
    -- response <- sendThrow "Kinesis EventSource Get" awsEnv getRecordsReq
    loadedEvents <- go (Just getRecordsReq)

    putStrLn $ "Got " <> show () <> " records from Kinesis"
    pure loadedEvents

-- TODO(Elaine): more error handling, dealing with shards, pagination, etc
-- we could do lightweight-ish full fledged error handling with ErrorT String IO [e]
-- then we could use AWS.sendEither to make stuff real nice

parseKinesisRecords :: (FromJSON e, MonadError KinesisException m) => AWS.GetRecordsResponse -> m [e]
parseKinesisRecords
  AWS.GetRecordsResponse'{childShards, millisBehindLatest, nextShardIterator, httpStatus, records} = do
    forM records $ \record ->
      modifyError KinesisUnmarshalException . hoistEither . eitherDecode $ record ^. record_data . lazy
