module Hydra.Node.Run where

import Hydra.Prelude hiding (fromList)

import Hydra.API.Server (Server (..), withAPIServer)
import Hydra.API.ServerOutput (ServerOutput (..))
import Hydra.Cardano.Api (
  ProtocolParametersConversionError,
 )
import Hydra.Chain (maximumNumberOfParties)
import Hydra.Chain.CardanoClient (QueryPoint (..), queryGenesisParameters)
import Hydra.Chain.Direct (loadChainContext, mkTinyWallet, withDirectChain)
import Hydra.Chain.Direct.State (initialChainState)
import Hydra.Chain.Offline (loadGenesisFile, withOfflineChain)
import Hydra.Events.AWS.Kinesis (exampleKinesisEventPair)
import Hydra.Events.UDP (exampleUDPSink)
import Hydra.HeadLogic (
  Environment (..),
 )
import Hydra.Ledger.Cardano qualified as Ledger
import Hydra.Ledger.Cardano.Configuration (
  Globals,
  newGlobals,
  newLedgerEnv,
  pparamsFromJson,
  readJsonFileThrow,
 )
import Hydra.Logging (Verbosity (..), traceWith, withTracer)
import Hydra.Logging.Messages (HydraLog (..))
import Hydra.Logging.Monitoring (withMonitoring)
import Hydra.Network.Message (Connectivity (..))
import Hydra.Node (
  chainStateHistory,
  connect,
  hydrate,
  initEnvironment,
  mkHydraNode,
  runHydraNode,
  wireChainInput,
  wireClientInput,
  wireNetworkInput,
 )
import Hydra.Node.Network (NetworkConfiguration (..), withNetwork)
import Hydra.Options (
  ChainConfig (..),
  DirectChainConfig (..),
  InvalidOptions (..),
  LedgerConfig (..),
  OfflineChainConfig (..),
  RunOptions (..),
  validateRunOptions, KinesisConfig (..), S3Config (..),
 )
import Hydra.Persistence (createPersistenceIncremental, eventPairFromPersistenceIncremental)
import Hydra.Events.AWS.S3 (exampleS3EventPair)

data ConfigurationException
  = ConfigurationException ProtocolParametersConversionError
  | InvalidOptionException InvalidOptions
  deriving stock (Show)

instance Exception ConfigurationException where
  displayException = \case
    InvalidOptionException MaximumNumberOfPartiesExceeded ->
      "Maximum number of parties is currently set to: " <> show maximumNumberOfParties
    InvalidOptionException CardanoAndHydraKeysMissmatch ->
      "Number of loaded cardano and hydra keys needs to match"
    ConfigurationException err ->
      "Incorrect protocol parameters configuration provided: " <> show err

run :: RunOptions -> IO ()
run opts = do
  either (throwIO . InvalidOptionException) pure $ validateRunOptions opts
  withTracer verbosity $ \tracer' -> do
    traceWith tracer' (NodeOptions opts)
    withMonitoring monitoringPort tracer' $ \tracer -> do
      env@Environment{party, otherParties, signingKey} <- initEnvironment opts
      -- Ledger
      pparams <- readJsonFileThrow pparamsFromJson (cardanoLedgerProtocolParametersFile ledgerConfig)
      globals <- getGlobalsForChain chainConfig
      withCardanoLedger pparams globals $ \ledger -> do
        -- Start hydra node wiring
        dryHydraNode <- mkHydraNode (contramap Node tracer) env ledger initialChainState
        -- Hydrate with event source and sinks
        persistence <- createPersistenceIncremental $ persistenceDir <> "/state"
        let (fileEventSource, filePersistenceSink) = eventPairFromPersistenceIncremental persistence
        let RunOptions
              { kinesisConfig = kinesisConfig@KinesisConfig{kinesisSourceEnabled}
              , s3Config = s3Config@S3Config{s3SourceEnabled}} = opts

        --TODO(Elaine): asum
        eventSource <- maybe (pure fileEventSource) snd $ find fst
              [ (kinesisSourceEnabled, fst <$> exampleKinesisEventPair kinesisConfig)
              , (s3SourceEnabled, fst <$> exampleS3EventPair s3Config)
              ]

        eventSinks <-
          sequence
            [ pure filePersistenceSink
            , exampleUDPSink "0.0.0.0" "3000"
            , snd <$> exampleKinesisEventPair kinesisConfig
            , snd <$> exampleS3EventPair s3Config
            ]
        -- Load events and hydrate sinks
        wetHydraNode <- hydrate eventSource eventSinks dryHydraNode

        -- Chain
        withChain <- prepareChainComponent tracer env chainConfig
        withChain (chainStateHistory wetHydraNode) (wireChainInput wetHydraNode) $ \chain -> do
          -- API
          apiPersistence <- createPersistenceIncremental $ persistenceDir <> "/server-output"
          withAPIServer apiHost apiPort party apiPersistence (contramap APIServer tracer) chain pparams (wireClientInput wetHydraNode) $ \server -> do
            -- Network
            let networkConfiguration = NetworkConfiguration{persistenceDir, signingKey, otherParties, host, port, peers, nodeId}
            withNetwork tracer (connectionMessages server) networkConfiguration (wireNetworkInput wetHydraNode) $ \network -> do
              -- Main loop
              connect chain network server wetHydraNode
                >>= runHydraNode
 where
  connectionMessages Server{sendOutput} = \case
    Connected nodeid -> sendOutput $ PeerConnected nodeid
    Disconnected nodeid -> sendOutput $ PeerDisconnected nodeid

  withCardanoLedger protocolParams globals action =
    let ledgerEnv = newLedgerEnv protocolParams
     in action (Ledger.cardanoLedger globals ledgerEnv)

  prepareChainComponent tracer Environment{party} = \case
    Offline cfg ->
      pure $ withOfflineChain cfg party
    Direct cfg -> do
      ctx <- loadChainContext cfg party
      wallet <- mkTinyWallet (contramap DirectChain tracer) cfg
      pure $ withDirectChain (contramap DirectChain tracer) cfg ctx wallet

  RunOptions
    { verbosity
    , monitoringPort
    , persistenceDir
    , chainConfig
    , ledgerConfig
    , host
    , port
    , peers
    , nodeId
    , apiHost
    , apiPort
    } = opts

getGlobalsForChain :: ChainConfig -> IO Globals
getGlobalsForChain = \case
  Offline OfflineChainConfig{ledgerGenesisFile} ->
    loadGenesisFile ledgerGenesisFile
      >>= newGlobals
  Direct DirectChainConfig{networkId, nodeSocket} ->
    queryGenesisParameters networkId nodeSocket QueryTip
      >>= newGlobals

identifyNode :: RunOptions -> RunOptions
identifyNode opt@RunOptions{verbosity = Verbose "HydraNode", nodeId} = opt{verbosity = Verbose $ "HydraNode-" <> show nodeId}
identifyNode opt = opt
