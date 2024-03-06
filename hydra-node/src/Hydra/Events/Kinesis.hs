{-# LANGUAGE BlockArguments #-}
module Hydra.Events.Kinesis where

import Hydra.Prelude

import Data.Aeson (encode)
import Hydra.Events (EventSink (..), HasEventId (..))
import Amazonka (Env, Base64 (Base64))
import qualified Amazonka as AWS
import qualified Amazonka.Kinesis as AWS
import qualified Amazonka.Kinesis.PutRecord as AWS

exampleKinesisSink :: ToJSON e => Env -> Text -> Text -> IO (EventSink e IO)
exampleKinesisSink awsEnv streamArn streamName = do
  pure $ EventSink $ \e -> do
    let putRecordReq = AWS.PutRecord'
          { explicitHashKey = Just . show $ getEventId e
          , sequenceNumberForOrdering = Nothing
          , streamARN = Just streamArn
          , streamName = Just streamName 
          , data' = Base64 . toStrict $ encode e --TODO(Elaine): is this just a constructor that represents a base64 encoded string?
          , partitionKey = show $ getEventId e
          }
    putStrLn $ "Sending event " <> show (getEventId e) <> " to Kinesis"
    response <- AWS.runResourceT $ AWS.send awsEnv putRecordReq --TODO(Elaine): error handling, response parsing
    putStrLn $ "Sent event " <> show (getEventId e) <> " to Kinesis"
