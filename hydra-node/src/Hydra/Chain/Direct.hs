{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Chain component implementation which uses directly the Node-to-Client
-- protocols to submit "hand-rolled" transactions.
module Hydra.Chain.Direct (
  NetworkMagic (NetworkMagic),
  module Hydra.Chain.Direct,
) where

import Hydra.Prelude

import Cardano.Api.UTxO (UTxO' (toMap))
import Cardano.Ledger.BaseTypes (epochInfoPure)
import Cardano.Ledger.Shelley.API (fromNominalDiffTimeMicro)
import Cardano.Ledger.Shelley.API qualified as Ledger
import Cardano.Ledger.Slot (EpochInfo, SlotNo (..))
import Cardano.Slotting.EpochInfo (EpochInfo (EpochInfo), epochInfoFirst, epochInfoSlotToUTCTime, fixedEpochInfo, hoistEpochInfo)
import Cardano.Slotting.Time (SystemStart (SystemStart), mkSlotLength, toRelativeTime)
import Control.Concurrent.Class.MonadSTM (
  newEmptyTMVar,
  newTQueueIO,
  putTMVar,
  readTQueue,
  takeTMVar,
  writeTQueue,
 )
import Control.Exception (IOException)
import Control.Monad.Trans.Except (runExcept)
import Data.Aeson qualified as Aeson
import Data.Time.Clock.POSIX (getPOSIXTime, systemToPOSIXTime, utcTimeToPOSIXSeconds)
import Hydra.Cardano.Api (
  Block (..),
  BlockInMode (..),
  CardanoMode,
  ChainPoint,
  ChainTip,
  ConsensusModeParams (..),
  EpochSlots (..),
  EraHistory (EraHistory),
  EraInMode (BabbageEraInCardanoMode),
  LocalChainSyncClient (..),
  LocalNodeClientProtocols (..),
  LocalNodeConnectInfo (..),
  NetworkId,
  SocketPath,
  StandardCrypto,
  Tx,
  TxId,
  TxInMode (..),
  TxValidationErrorInMode,
  chainTipToChainPoint,
  connectToLocalNode,
  getTxBody,
  getTxId,
  readFileJSON,
  toLedgerUTxO,
 )
import Hydra.Cardano.Api.TxIn (toLedgerTxIn)
import Hydra.Cardano.Api.TxOut (toLedgerTxOut)
import Hydra.Chain (
  ChainComponent,
  ChainEvent (Observation, Tick, observedTx),
  ChainStateHistory,
  OnChainTx (OnCollectComTx, OnCommitTx, OnInitTx, contestationPeriod, headId, parties),
  PostTxError (..),
  chainSlot,
  chainTime,
  committed,
  currentState,
  initHistory,
  newChainState,
  party,
 )
import Hydra.Chain.CardanoClient (
  QueryPoint (..),
  queryEraHistory,
  queryProtocolParameters,
  querySystemStart,
  queryTip,
  queryUTxO,
 )
import Hydra.Chain.Direct.Handlers (
  ChainSyncHandler,
  DirectChainLog (..),
  chainSyncHandler,
  mkChain,
  mkFakeL1Chain,
  newLocalChainState,
  onRollBackward,
  onRollForward,
 )
import Hydra.Chain.Direct.ScriptRegistry (queryScriptRegistry)
import Hydra.Chain.Direct.State (
  ChainContext (..),
  ChainState (Idle, Open),
  ChainStateAt (..),
  OpenState (OpenState, headId),
  initialChainState,
  observeCommit,
  openThreadOutput,
 )
import Hydra.Chain.Direct.TimeHandle (queryTimeHandle)
import Hydra.Chain.Direct.Tx (OpenThreadOutput (OpenThreadOutput, openThreadUTxO))
import Hydra.Chain.Direct.Util (
  readKeyPair,
 )
import Hydra.Chain.Direct.Wallet (
  TinyWallet (..),
  WalletInfoOnChain (..),
  newTinyWallet,
 )
import Hydra.Data.ContestationPeriod (contestationPeriodFromDiffTime)
import Hydra.Ledger (ChainSlot (ChainSlot), UTxOType)
import Hydra.Ledger.Cardano.Configuration (readJsonFileThrow)
import Hydra.Logging (Tracer, traceWith)
import Hydra.Options (ChainConfig (..), OfflineConfig (..))
import Hydra.Party (Party)
import Hydra.Plutus.Extras (posixFromUTCTime)
import Ouroboros.Consensus.HardFork.History (interpretQuery, mkInterpreter, neverForksSummary, wallclockToSlot)
import Ouroboros.Consensus.HardFork.History qualified as Consensus
import Ouroboros.Consensus.Util.Time (nominalDelay)
import Ouroboros.Network.Magic (NetworkMagic (..))
import Ouroboros.Network.Protocol.ChainSync.Client (
  ChainSyncClient (..),
  ClientStIdle (..),
  ClientStIntersect (..),
  ClientStNext (..),
 )
import Ouroboros.Network.Protocol.LocalTxSubmission.Client (
  LocalTxClientStIdle (..),
  LocalTxSubmissionClient (..),
  SubmitResult (..),
 )

import Hydra.HeadId (HeadId (..))

-- | Build the 'ChainContext' from a 'ChainConfig' and additional information.
loadChainContext ::
  ChainConfig ->
  -- | Hydra party of our hydra node.
  Party ->
  -- | Transaction id at which to look for Hydra scripts.
  TxId ->
  IO ChainContext
loadChainContext config party hydraScriptsTxId = do
  (vk, _) <- readKeyPair cardanoSigningKey
  scriptRegistry <- queryScriptRegistry networkId nodeSocket hydraScriptsTxId
  pure $
    ChainContext
      { networkId
      , ownVerificationKey = vk
      , ownParty = party
      , scriptRegistry
      }
 where
  DirectChainConfig
    { networkId
    , nodeSocket
    , cardanoSigningKey
    } = config

mkTinyWallet ::
  Tracer IO DirectChainLog ->
  ChainConfig ->
  IO (TinyWallet IO)
mkTinyWallet tracer config = do
  keyPair <- readKeyPair cardanoSigningKey
  newTinyWallet (contramap Wallet tracer) networkId keyPair queryWalletInfo queryEpochInfo
 where
  DirectChainConfig{networkId, nodeSocket, cardanoSigningKey} = config

  queryEpochInfo = toEpochInfo <$> queryEraHistory networkId nodeSocket QueryTip

  queryWalletInfo queryPoint address = do
    point <- case queryPoint of
      QueryAt point -> pure point
      QueryTip -> queryTip networkId nodeSocket
    walletUTxO <- Ledger.unUTxO . toLedgerUTxO <$> queryUTxO networkId nodeSocket QueryTip [address]
    pparams <- queryProtocolParameters networkId nodeSocket QueryTip
    systemStart <- querySystemStart networkId nodeSocket QueryTip
    epochInfo <- queryEpochInfo
    pure $ WalletInfoOnChain{walletUTxO, pparams, systemStart, epochInfo, tip = point}

  toEpochInfo :: EraHistory CardanoMode -> EpochInfo (Either Text)
  toEpochInfo (EraHistory _ interpreter) =
    hoistEpochInfo (first show . runExcept) $
      Consensus.interpreterToEpochInfo interpreter

withOfflineChain ::
  Tracer IO DirectChainLog -> -- TODO(ELAINE): change type maybe ?
  OfflineConfig ->
  _ ->
  ChainContext ->
  HeadId ->
  -- | Last known chain state as loaded from persistence.
  ChainStateHistory Tx ->
  ChainComponent Tx IO a
withOfflineChain tracer OfflineConfig{initialUTxOFile, ledgerGenesisFile} globals@Ledger.Globals{systemStart} ctx ownHeadId chainStateHistory callback action = do
  localChainState <- newLocalChainState chainStateHistory
  let chainHandle = mkFakeL1Chain localChainState tracer ctx ownHeadId callback

  -- L2 ledger normally has fixed epoch info based on slot length from protocol params
  -- we're getting it from gen params here, it should match, but this might motivate generating shelleygenesis based on protocol params

  tickForeverAction <- case ledgerGenesisFile of
    Just filePath -> do
      Ledger.ShelleyGenesis{sgSystemStart, sgSlotLength, sgEpochLength} <-
        readJsonFileThrow (parseJSON @(Ledger.ShelleyGenesis StandardCrypto)) filePath
      let slotLengthNominalDiffTime = fromNominalDiffTimeMicro sgSlotLength
          slotLength = mkSlotLength slotLengthNominalDiffTime

      let interpreter = mkInterpreter $ neverForksSummary sgEpochLength slotLength

      let slotFromUTCTime :: HasCallStack => UTCTime -> Either Consensus.PastHorizonException ChainSlot
          slotFromUTCTime utcTime = do
            let relativeTime = toRelativeTime (SystemStart sgSystemStart) utcTime
            case interpretQuery interpreter (wallclockToSlot relativeTime) of
              Left pastHorizonEx ->
                Left pastHorizonEx
              Right (SlotNo slotNoWord64, _timeSpentInSlot, _timeLeftInSlot) ->
                Right . ChainSlot . fromIntegral @Word64 @Natural $ slotNoWord64

      let tickForever :: IO ()
          tickForever = forever $ do
            chainTime <- getCurrentTime
            -- NOTE(Elaine): this shouldn't happen in offline mode, we should not construct an era history that ever ends
            chainSlot <- either throwIO pure . slotFromUTCTime $ chainTime
            callback $ Tick{chainTime = chainTime, chainSlot}

            -- NOTE(Elaine): this is just realToFrac, not sure if better etiquette to import or use directly
            threadDelay $ nominalDelay slotLengthNominalDiffTime
      pure tickForever
    Nothing -> do
      let epochInfo@EpochInfo{} = epochInfoPure globals
          initialSlot = runIdentity $ epochInfoFirst epochInfo 0

          nextTick upcomingSlot = do
            let timeToSleepUntil = runIdentity $ epochInfoSlotToUTCTime epochInfo systemStart upcomingSlot
            sleepDelay <- diffUTCTime timeToSleepUntil <$> getCurrentTime
            threadDelay $ nominalDelay sleepDelay
            callback $
              Tick
                { chainTime = timeToSleepUntil
                , chainSlot = ChainSlot . fromIntegral @Word64 @Natural $ unSlotNo upcomingSlot
                }

          tickForever = traverse_ nextTick [initialSlot ..]

      pure tickForever

  res <-
    race
      tickForeverAction
      (action chainHandle)

  case res of
    Left () -> error "'connectTo' cannot terminate but did?"
    Right a -> pure a

withDirectChain ::
  Tracer IO DirectChainLog ->
  ChainConfig ->
  ChainContext ->
  TinyWallet IO ->
  -- | Chain state loaded from persistence.
  ChainStateHistory Tx ->
  ChainComponent Tx IO a
withDirectChain tracer config ctx wallet chainStateHistory callback action = do
  -- Last known point on chain as loaded from persistence.
  let persistedPoint = recordedAt (currentState chainStateHistory)
  queue <- newTQueueIO
  -- Select a chain point from which to start synchronizing
  chainPoint <- maybe (queryTip networkId nodeSocket) pure $ do
    (min <$> startChainFrom <*> persistedPoint)
      <|> persistedPoint
      <|> startChainFrom

  let getTimeHandle = queryTimeHandle networkId nodeSocket
  localChainState <- newLocalChainState chainStateHistory
  let chainHandle =
        mkChain
          tracer
          getTimeHandle
          wallet
          ctx
          localChainState
          (submitTx queue)

  let handler = chainSyncHandler tracer callback getTimeHandle ctx localChainState
  res <-
    race
      ( handle onIOException $
          connectToLocalNode
            connectInfo
            (clientProtocols chainPoint queue handler)
      )
      (action chainHandle)
  case res of
    Left () -> error "'connectTo' cannot terminate but did?"
    Right a -> pure a
 where
  DirectChainConfig{networkId, nodeSocket, startChainFrom} = config

  connectInfo =
    LocalNodeConnectInfo
      { -- REVIEW: This was 432000 before, but all usages in the
        -- cardano-node repository are using this value. This is only
        -- relevant for the Byron era.
        localConsensusModeParams = CardanoModeParams (EpochSlots 21600)
      , localNodeNetworkId = networkId
      , localNodeSocketPath = nodeSocket
      }

  clientProtocols point queue handler =
    LocalNodeClientProtocols
      { localChainSyncClient = LocalChainSyncClient $ chainSyncClient handler wallet point
      , localTxSubmissionClient = Just $ txSubmissionClient tracer queue
      , localStateQueryClient = Nothing
      , localTxMonitoringClient = Nothing
      }

  submitTx queue tx = do
    response <- atomically $ do
      response <- newEmptyTMVar
      writeTQueue queue (tx, response)
      return response
    atomically (takeTMVar response)
      >>= maybe (pure ()) throwIO

  onIOException :: IOException -> IO ()
  onIOException ioException =
    throwIO $
      ConnectException
        { ioException
        , nodeSocket
        , networkId
        }

data ConnectException = ConnectException
  { ioException :: IOException
  , nodeSocket :: SocketPath
  , networkId :: NetworkId
  }
  deriving stock (Show)

instance Exception ConnectException

-- | Thrown when the user-provided custom point of intersection is unknown to
-- the local node. This may happen if users shut down their node quickly after
-- starting them and hold on a not-so-stable point of the chain. When they turn
-- the node back on, that point may no longer exist on the network if a fork
-- with deeper roots has been adopted in the meantime.
newtype IntersectionNotFoundException = IntersectionNotFound
  { requestedPoint :: ChainPoint
  }
  deriving stock (Show)

instance Exception IntersectionNotFoundException

-- | The block type used in the node-to-client protocols.
type BlockType = BlockInMode CardanoMode

chainSyncClient ::
  forall m.
  (MonadSTM m, MonadThrow m) =>
  ChainSyncHandler m ->
  TinyWallet m ->
  ChainPoint ->
  ChainSyncClient BlockType ChainPoint ChainTip m ()
chainSyncClient handler wallet startingPoint =
  ChainSyncClient $
    pure $
      SendMsgFindIntersect
        [startingPoint]
        ( clientStIntersect
            (\_ -> throwIO (IntersectionNotFound startingPoint))
        )
 where
  clientStIntersect ::
    (ChainPoint -> m (ClientStIdle BlockType ChainPoint ChainTip m ())) ->
    ClientStIntersect BlockType ChainPoint ChainTip m ()
  clientStIntersect onIntersectionNotFound =
    ClientStIntersect
      { recvMsgIntersectFound = \_ _ ->
          ChainSyncClient (pure clientStIdle)
      , recvMsgIntersectNotFound =
          ChainSyncClient . onIntersectionNotFound . chainTipToChainPoint
      }

  clientStIdle :: ClientStIdle BlockType ChainPoint ChainTip m ()
  clientStIdle = SendMsgRequestNext clientStNext (pure clientStNext)

  clientStNext :: ClientStNext BlockType ChainPoint ChainTip m ()
  clientStNext =
    ClientStNext
      { recvMsgRollForward = \blockInMode _tip -> ChainSyncClient $ do
          case blockInMode of
            BlockInMode _ (Block header txs) BabbageEraInCardanoMode -> do
              -- Update the tiny wallet
              update wallet header txs
              -- Observe Hydra transactions
              onRollForward handler header txs
              pure clientStIdle
            _ ->
              -- NOTE: We are just ignoring different era blocks. It's not
              -- entirely clear if we would reach this point on a "next-era"
              -- network (e.g. Conway) or just have a handshake problem before.
              pure clientStIdle
      , recvMsgRollBackward = \point _tip -> ChainSyncClient $ do
          -- Re-initialize the tiny wallet
          reset wallet
          -- Rollback main chain sync handler
          onRollBackward handler point
          pure clientStIdle
      }

txSubmissionClient ::
  forall m.
  (MonadSTM m, MonadDelay m) =>
  Tracer m DirectChainLog ->
  TQueue m (Tx, TMVar m (Maybe (PostTxError Tx))) ->
  LocalTxSubmissionClient (TxInMode CardanoMode) (TxValidationErrorInMode CardanoMode) m ()
txSubmissionClient tracer queue =
  LocalTxSubmissionClient clientStIdle
 where
  clientStIdle :: m (LocalTxClientStIdle (TxInMode CardanoMode) (TxValidationErrorInMode CardanoMode) m ())
  clientStIdle = do
    (tx, response) <- atomically $ readTQueue queue
    let txId = getTxId $ getTxBody tx
    traceWith tracer PostingTx{txId}
    pure $
      SendMsgSubmitTx
        (TxInMode tx BabbageEraInCardanoMode)
        ( \case
            SubmitSuccess -> do
              traceWith tracer PostedTx{txId}
              atomically (putTMVar response Nothing)
              clientStIdle
            SubmitFail err -> do
              -- XXX: Very complicated / opaque show instance and no unpacking
              -- possible because of missing data constructors from cardano-api
              let postTxError = FailedToPostTx{failureReason = show err}
              traceWith tracer PostingFailed{tx, postTxError}
              -- NOTE: Delay callback in case our transaction got invalidated
              -- because of a transaction seen in a block. This gives the
              -- observing side of the chain layer time to process the
              -- transaction and business logic might even ignore this error.
              threadDelay 1
              atomically (putTMVar response (Just postTxError))
              clientStIdle
        )
