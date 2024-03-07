{-# LANGUAGE BlockArguments #-}

module Hydra.Events.Kinesis where

import Hydra.Prelude

import Amazonka (Base64 (Base64), Env)
import Amazonka qualified as AWS
import Amazonka.Kinesis qualified as AWS
import Amazonka.Kinesis.GetRecords qualified as AWS
import Amazonka.Kinesis.Lens
import Amazonka.Kinesis.PutRecord qualified as AWS
import Control.Lens (lazy, view)
import Data.Aeson (encode)
import Data.Aeson.Decoding (eitherDecode)
import Hydra.Events (EventSink (..), EventSource (..), HasEventId (..))
import System.IO qualified as IO

import Hydra.Options (KinesisConfig (..))

-- TODO(Elaine): before using in production we'd want to make this type much more granular

-- | An exception that can be thrown by the Kinesis sink or source
newtype KinesisException = KinesisSinkException String
  deriving stock (Show, Eq)
  deriving anyclass (Exception)

exampleKinesisEventPair :: (ToJSON e, FromJSON e) => KinesisConfig -> IO (EventSource e IO, EventSink e IO)
exampleKinesisEventPair kinesisConfig = do
  awsLogger <- AWS.newLogger AWS.Debug IO.stdout -- TODO(Elaine): we can use our own nice logging
  awsDiscoveredEnv <- AWS.newEnv AWS.discover
  let awsEnv = awsDiscoveredEnv{AWS.logger = awsLogger}
  source <- exampleKinesisSource awsEnv kinesisConfig
  sink <- exampleKinesisSink awsEnv kinesisConfig
  pure (source, sink)

exampleKinesisSink :: ToJSON e => Env -> KinesisConfig -> IO (EventSink e IO)
exampleKinesisSink awsEnv KinesisConfig{kinesisStreamName, kinesisStreamARN} = do
  pure $ EventSink \e -> do
    let putRecordReq =
          AWS.PutRecord'
            { explicitHashKey = Just . show $ getEventId e
            , sequenceNumberForOrdering = Nothing
            , streamARN = Just kinesisStreamARN
            , streamName = Just kinesisStreamName
            , data' = Base64 . toStrict $ encode e -- TODO(Elaine): is this just a constructor that represents a base64 encoded string?
            , partitionKey = show $ getEventId e
            }
    putStrLn $ "Sending event " <> show (getEventId e) <> " to Kinesis"
    response <- AWS.runResourceT $ AWS.send awsEnv putRecordReq -- TODO(Elaine): error handling, response parsing
    putStrLn $ "Sent event " <> show (getEventId e) <> " to Kinesis"

exampleKinesisSource :: FromJSON e => Env -> KinesisConfig -> IO (EventSource e IO)
exampleKinesisSource awsEnv KinesisConfig{kinesisStreamARN} = do
  pure $ EventSource do
    let getRecordsReq =
          AWS.GetRecords'
            { limit = Nothing -- TODO(Elaine): we definitely need to tune a particular limit
            , shardIterator = ""
            , streamARN = Just kinesisStreamARN
            }
    putStrLn "Getting records from Kinesis"
    response <- AWS.runResourceT $ AWS.send awsEnv getRecordsReq

    loadedEvents <- either (throwIO . KinesisSinkException . mappend "Failed to parse kinesis response: ") pure . parseKinesisRecords $ response

    putStrLn $ "Got " <> show () <> " records from Kinesis"
    pure loadedEvents

-- TODO(Elaine): more error handling, dealing with shards, pagination, etc
-- we could do lightweight-ish full fledged error handling with ErrorT String IO [e]
-- then we could use AWS.sendEither to make stuff real nice

parseKinesisRecords :: FromJSON e => AWS.GetRecordsResponse -> Either String [e]
parseKinesisRecords
  AWS.GetRecordsResponse'{childShards, millisBehindLatest, nextShardIterator, httpStatus, records} = do
    mapM (eitherDecode . view (record_data . lazy)) records
