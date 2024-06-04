module Hydra.Events.UDP where

import Hydra.Prelude

import Data.Aeson (encode)
import Hydra.Events (EventSink (..), HasEventId (..))
import Network.Socket (HostName, ServiceName)
import Network.UDP (clientSocket, send)
import Codec.Serialise (serialise)
import Codec.CBOR.JSON (encodeValue)
import Codec.CBOR.Write (toStrictByteString)

exampleUDPSink :: (HasEventId e, ToJSON e) => HostName -> ServiceName -> IO (EventSink e IO)
exampleUDPSink addr port = do
  socket <- clientSocket addr port False
  pure $ EventSink $ \e -> do
    send socket . toStrictByteString . encodeValue . toJSON $ e
    putStrLn $ "Sending event " <> show (getEventId e) <> " to UDP"
