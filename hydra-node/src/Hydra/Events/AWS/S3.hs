{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE DerivingVia #-}
module Hydra.Events.AWS.S3 where

import Hydra.Prelude

import Amazonka (Env)
import Amazonka qualified as AWS
import Amazonka.S3 qualified as AWS
import Control.Lens.Operators ((^.), (.~), (?~))
import Data.Aeson (encode)
import Data.Aeson.Decoding (eitherDecode)
import Data.Time.Clock qualified as Time
import Data.Time.Clock.POSIX (getPOSIXTime)
import Hydra.Events (EventSink (..), EventSource (..), HasEventId (..), EventId)
import System.IO qualified as IO
import Control.Monad.Except

import Hydra.Options (S3Config(..))
import qualified Amazonka.S3.Lens as AWS
import Control.Lens (_Just, each, (^?))
import Control.Lens ((^..))
import Control.Monad.Fix
import Control.Monad.Class.MonadAsync (Concurrently(runConcurrently))
import Control.Concurrent.Class.MonadSTM
import Control.Concurrent (writeList2Chan, newChan, readChan)
import Control.Lens.Iso (lazy)
import Conduit (runConduit, tryC, (.|))
import Data.Conduit.Combinators (sinkList)
import Hydra.HeadLogic.Outcome (StateChanged(..))
import Gummiworm.Archive (serializeArchiveTransactions, readTransactionsFrom)

-- | An exception that can be thrown by the S3 sink or source
data S3Exception = S3UnmarshalException String | S3AWSError String AWS.Error
  deriving stock (Show)
  deriving anyclass (Exception)

newtype S3 a = S3 { unS3 :: ReaderT S3Env (ExceptT S3Exception IO) a }
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadThrow, MonadError S3Exception, MonadReader S3Env, MonadFix)

-- can replace instances for debugging, stack trace, etc

type S3Env = (AWS.Env, S3Config)

runS3 :: Env -> S3Config -> S3 a -> IO (Either S3Exception a)
runS3 awsEnv awsConfig action = runExceptT $ runReaderT (unS3 action) (awsEnv, awsConfig)

runS3ThrowIO :: Env -> S3Config -> S3 b -> IO b
runS3ThrowIO awsEnv awsConfig action = either throwIO pure =<< runS3 awsEnv awsConfig action

sendThrow :: (MonadError S3Exception m, MonadIO m, AWS.AWSRequest a,  Typeable a, Typeable (AWS.AWSResponse a)) => String -> Env -> a -> m (AWS.AWSResponse a)
sendThrow task env sendRecord = do
  runResourceThrowS3Error $ AWS.sendEither env sendRecord
 where
  runResourceThrowS3Error = wrapErrorS3 <=< runResourceS3 . fmap hoistEither
  wrapErrorS3 = modifyError (S3AWSError $ "AWS Exception during " <> task)
  runResourceS3 = liftIO . AWS.runResourceT

--TODO(Elaine): check this parsing
s3EventPath :: EventId -> S3 AWS.ObjectKey
s3EventPath eventId = do
  (_, S3Config{s3ObjectPath}) <- ask
  posixTime <- liftIO $ do
    truncate <$> getPOSIXTime
  pure $
    AWS.ObjectKey ""
      & AWS.objectKey_keyPrefix '/' .~ s3ObjectPath
      & AWS.objectKey_keyName '/' .~ show posixTime

exampleS3EventPair :: S3Config -> IO (EventSource _e IO, EventSink _e IO)
exampleS3EventPair s3Config@S3Config{s3SinkEnabled, s3SourceEnabled}= do
  awsLogger <- AWS.newLogger AWS.Debug IO.stdout -- TODO(Elaine): we can use our own nice logging
  awsDiscoveredEnv <- AWS.newEnv AWS.discover
  let awsEnv = awsDiscoveredEnv{AWS.logger = awsLogger}
  
  source <- if s3SourceEnabled then runS3ThrowIO awsEnv s3Config exampleS3Source else pure (EventSource $ pure [])
  sink <- if s3SinkEnabled then runS3ThrowIO awsEnv s3Config exampleS3Sink else pure (EventSink $ void . pure)

  pure
    ( EventSource $ runS3ThrowIO awsEnv s3Config (getEvents source)
    , EventSink $ runS3ThrowIO awsEnv s3Config . putEvent sink
    )

exampleS3Sink :: S3 (EventSink _ S3)
exampleS3Sink = do
  (awsEnv, S3Config{..}) <- ask
  --TODO(Elaine): MonadSTM classy
  eventQueue <- liftIO newTQueueIO
  -- store a bunch of events in a queue till it gets full, then drain it
  -- it's fine if we lose events on crash, this demo doesn't need to be durable
  -- besides, that's why we have multiple sink

  -- we can do this based on time, or we can do it based on number of events
  -- for now, a simple solution is to just mod the event ID
  -- NB: This seems to fire every 100 ms. Not sure if that can be controlled.
  initialTime <- liftIO getCurrentTime
  currentTime <- newIORef initialTime
  pure $ EventSink $ \e -> do
    eventPath <- s3EventPath (getEventId e)

    liftIO . atomically $ writeTQueue eventQueue e
    newTime <- liftIO getCurrentTime
    -- diffUTCTime deadline <$> getCurrentTime
    elapsed <- liftIO $ diffUTCTime <$> pure newTime <*> readIORef currentTime
    -- when (getEventId e `mod` 99 == 0) $ do -- What was here. Fire every 100 events.
    when (elapsed >= 10) $ do -- TODO: Make time variable.
      liftIO $ writeIORef currentTime newTime
      --things can change between when we check and when we flush, but for current usage, poses no issue
      --TODO(Elaine): be careful, because the title of the archive won't always correspond exactly to which events r included
      flushedEvents <- liftIO . atomically $ flushTQueue eventQueue
      let flushedTransactions = mapMaybe (\case TransactionAppliedToLocalUTxO{tx, newLocalUTxO, stateChangeID} -> Just (stateChangeID, tx); _ -> Nothing) flushedEvents
      let numTXs = length flushedTransactions
      liftIO . putStrLn $ "Flushing " <> show (numTXs) <> " events to S3"
      when (numTXs > 0) do
        serializedArchiveTransactions <- liftIO $ serializeArchiveTransactions . fmap snd $ flushedTransactions
        let putObjectReq = AWS.newPutObject (AWS.BucketName s3BucketName) eventPath (AWS.toBody serializedArchiveTransactions )-- (Base64 $ encode e)

        response <- sendThrow "S3 Event Sink putEvent" awsEnv putObjectReq
        liftIO $ putStrLn "Flushed events to S3"

exampleS3Source :: S3 (EventSource _ S3)
exampleS3Source = do
  (awsEnv, S3Config{..}) <- ask
  pure $ EventSource do
    let go Nothing = pure []
        go (Just page) = do
            liftIO . putStrLn $ "Getting events from S3"
            response <- sendThrow "S3 Event Source getEvents" awsEnv page
            let foundKeys = response ^.. AWS.listObjectsV2Response_contents . _Just . each . AWS.object_key
            let nextPageReq = AWS.page page response
            liftIO $ threadDelay 5_000_000 -- NOTE: this is threadDelay not from hydra prelude

            rec futureFoundKeys <- go nextPageReq

            pure $ foundKeys <> futureFoundKeys
    let listObjsReq =
            AWS.newListObjectsV2 (AWS.BucketName s3BucketName)
                & AWS.listObjectsV2_prefix ?~ s3ObjectPath
    eventObjKeys <- go (Just listObjsReq)

    --TODO(Elaine): parallelize
    --TODO(Elaine): rate limit
    events <- forM eventObjKeys $ \obj ->
        let getObjectReq = AWS.newGetObject (AWS.BucketName s3BucketName) obj
        in do
            liftIO . putStrLn $ "Getting event archive " <> show obj <> " from S3"
            response <- sendThrow "S3 Event Source getEvent" awsEnv getObjectReq
            --TODO(Elaine): conduit is so annoying dude
            responseBody <- fmap mconcat . runConduit . flip AWS.sinkBody sinkList $ response ^. AWS.getObjectResponse_body
            
            modifyError (S3UnmarshalException . show) . hoistEither <=< liftIO . readTransactionsFrom . toLazy $ responseBody

    liftIO . putStrLn $ "Got " <> show (length events) <> " events from S3"
    pure []
