module Hydra.Events.UDP where

import Hydra.Prelude

import Data.Aeson (encode)
import Hydra.Events (EventSink (..), HasEventId (..))
import Network.Socket (HostName, ServiceName)
import Network.UDP (clientSocket, send)

-- To build a sink:
--

exampleUDPSink :: (HasEventId e, ToJSON e) => HostName -> ServiceName -> IO (EventSink e IO)
exampleUDPSink addr port = do
  socket <- clientSocket addr port False
  pure $ EventSink $ \e -> do
    send socket (toStrict $ encode e)
    putStrLn $ "Sending event " <> show (getEventId e) <> " to UDP"

-- To build a source:

-- TODO: Do we want this? I'd assume probably not for a first demo
exampleUDPSource :: a
exampleUDPSource = undefined
