module Hydra.Events.UDP where

import Hydra.Prelude

import Data.Aeson (encode)
import Hydra.Events (EventSink (..), HasEventId (..))
import Network.Socket (HostName, ServiceName)
import Network.UDP (clientSocket, send)

-- To build a sink:
--

exampleUDPSink :: (HasEventId e, ToJSON e) => HostName -> ServiceName -> EventSink e IO
exampleUDPSink addr port =
  EventSink $ \e -> do
    socket <- clientSocket addr port False
    send socket (toStrict $ encode e)
    putStrLn $ "Sending event " <> show (getEventId e) <> " to UDP"

-- To build a source:

-- TODO: Do we want this? I'd assume probably not for a first demo
exampleUDPSource :: a
exampleUDPSource = undefined
