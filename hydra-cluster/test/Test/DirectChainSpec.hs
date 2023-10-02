{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Test.DirectChainSpec where

import Hydra.Prelude
import Test.Hydra.Prelude

import Cardano.Api.UTxO (UTxO' (UTxO, toMap))
import CardanoClient (
  CardanoClient (..),
  QueryType (..),
  QueryPoint (QueryTip),
  buildAddress,
  queryUTxO,
  submitTx,
  waitForUTxO,
 )
import CardanoNode (NodeLog, RunningNode (..), withCardanoNodeDevnet)
import Control.Concurrent.STM (newEmptyTMVarIO, takeTMVar)
import Control.Concurrent.STM.TMVar (putTMVar)
import Hydra.Cardano.Api (
  ChainPoint (..),
  CtxUTxO,
  Key (SigningKey),
  KeyWitnessInCtx (KeyWitnessForSpending),
  PaymentKey,
  TxOut,
  WitCtxTxIn,
  Witness,
  lovelaceToValue,
  signTx,
  txOutValue,
  unFile,
  pattern KeyWitness,
 )
import Hydra.Chain (
  Chain (Chain, draftCommitTx, postTx),
  ChainEvent (..),
  HeadParameters (..),
  OnChainTx (..),
  PostChainTx (..),
  PostTxError (..),
  initHistory,
 )
import Hydra.Chain.Direct (
  IntersectionNotFoundException (..),
  loadChainContext,
  mkTinyWallet,
  withDirectChain,
 )
import Hydra.Chain.Direct.Handlers (DirectChainLog)
import Hydra.Chain.Direct.ScriptRegistry (queryScriptRegistry)
import Hydra.Chain.Direct.State (ChainContext (..), initialChainState)
import Hydra.Cluster.Faucet (
  FaucetLog,
  publishHydraScriptsAs,
  seedFromFaucet,
  seedFromFaucet_,
 )
import Hydra.Cluster.Fixture (
  Actor (Alice, Bob, Carol, Faucet),
  alice,
  aliceSk,
  bob,
  carol,
  cperiod,
 )
import Hydra.Cluster.Util (chainConfigFor, keysFor)
import Hydra.ContestationPeriod (ContestationPeriod)
import Hydra.Crypto (aggregate, sign)
import Hydra.Ledger (IsTx (..))
import Hydra.Ledger.Cardano (Tx, genKeyPair)
import Hydra.Logging (Tracer, nullTracer, showLogsOnFailure)
import Hydra.Options (
  ChainConfig (..),
  toArgNetworkId,
 )
import Hydra.Party (Party)
import Hydra.Snapshot (ConfirmedSnapshot (..), Snapshot (..))
import System.Process (proc, readCreateProcess)
import Test.QuickCheck (generate)

spec :: Spec
spec = around showLogsOnFailure $ do
  it "can init and abort a head given nothing has been committed" $ \tracer -> do
    withTempDir "hydra-cluster" $ \tmp -> do
      withCardanoNodeDevnet (contramap FromNode tracer) tmp $ \cardanoClient -> do
        (aliceCardanoVk, _) <- keysFor Alice
        seedFromFaucet_ cardanoClient aliceCardanoVk 100_000_000 (contramap FromFaucet tracer)
        hydraScriptsTxId <- publishHydraScriptsAs cardanoClient Faucet
        -- Alice setup
        aliceChainConfig <- chainConfigFor Alice tmp cardanoClient [Bob, Carol] cperiod
        aliceChainContext <- loadChainContext aliceChainConfig alice [bob, carol] hydraScriptsTxId
        withDirectChainTest (contramap (FromDirectChain "alice") tracer) aliceChainConfig aliceChainContext $
          \aliceChain@DirectChainTest{postTx} -> do
            -- Bob setup
            bobChainConfig <- chainConfigFor Bob tmp cardanoClient [Alice, Carol] cperiod
            bobChainContext <- loadChainContext bobChainConfig bob [alice, carol] hydraScriptsTxId
            withDirectChainTest nullTracer bobChainConfig bobChainContext $
              \bobChain@DirectChainTest{} -> do
                -- Scenario
                postTx $ InitTx $ HeadParameters cperiod [alice, bob, carol]
                aliceChain `observesInTimeSatisfying` hasInitTxWith cperiod [alice, bob, carol]
                bobChain `observesInTimeSatisfying` hasInitTxWith cperiod [alice, bob, carol]

                postTx $ AbortTx mempty

                aliceChain `observesInTime` OnAbortTx
                bobChain `observesInTime` OnAbortTx

  it "can init and abort a 2-parties head after one party has committed" $ \tracer -> do
    withTempDir "hydra-cluster" $ \tmp -> do
      withCardanoNodeDevnet (contramap FromNode tracer) tmp $ \cardanoClient -> do
        hydraScriptsTxId <- publishHydraScriptsAs cardanoClient Faucet
        -- Alice setup
        (aliceCardanoVk, _) <- keysFor Alice
        seedFromFaucet_ cardanoClient aliceCardanoVk 100_000_000 (contramap FromFaucet tracer)
        aliceChainConfig <- chainConfigFor Alice tmp cardanoClient [Bob, Carol] cperiod
        aliceChainContext <- loadChainContext aliceChainConfig alice [bob, carol] hydraScriptsTxId
        withDirectChainTest (contramap (FromDirectChain "alice") tracer) aliceChainConfig aliceChainContext $
          \aliceChain@DirectChainTest{postTx} -> do
            -- Bob setup
            bobChainConfig <- chainConfigFor Bob tmp cardanoClient [Alice, Carol] cperiod
            bobChainContext <- loadChainContext bobChainConfig bob [alice, carol] hydraScriptsTxId
            withDirectChainTest (contramap (FromDirectChain "bob") tracer) bobChainConfig bobChainContext $
              \bobChain@DirectChainTest{} -> do
                -- Scenario
                let aliceCommitment = 66_000_000
                -- Mimic "external commit" by using different keys for Alice.
                (aliceExternalVk, aliceExternalSk) <- generate genKeyPair

                aliceUTxO <- seedFromFaucet cardanoClient aliceExternalVk aliceCommitment (contramap FromFaucet tracer)

                postTx $ InitTx $ HeadParameters cperiod [alice, bob, carol]
                aliceChain `observesInTimeSatisfying` hasInitTxWith cperiod [alice, bob, carol]
                bobChain `observesInTimeSatisfying` hasInitTxWith cperiod [alice, bob, carol]

                externalCommit cardanoClient aliceChain aliceExternalSk aliceUTxO

                aliceChain `observesInTime` OnCommitTx alice aliceUTxO
                bobChain `observesInTime` OnCommitTx alice aliceUTxO

                postTx $ AbortTx aliceUTxO
                --
                aliceChain `observesInTime` OnAbortTx
                bobChain `observesInTime` OnAbortTx

                let aliceExternalAddress = buildAddress aliceExternalVk $ networkIdClientOnline cardanoClient

                -- Expect that Alice got her committed value back to her
                -- external address
                utxo <- queryUTxOClientOnline cardanoClient queryTypeTip [aliceExternalAddress]
                let aliceValues = txOutValue <$> toList utxo
                aliceValues `shouldContain` [lovelaceToValue aliceCommitment]

  it "cannot abort a non-participating head" $ \tracer ->
    withTempDir "hydra-cluster" $ \tmp -> do
      withCardanoNodeDevnet (contramap FromNode tracer) tmp $ \cardanoClient -> do
        hydraScriptsTxId <- publishHydraScriptsAs cardanoClient Faucet
        -- Alice setup
        (aliceCardanoVk, _) <- keysFor Alice
        seedFromFaucet_ cardanoClient aliceCardanoVk 100_000_000 (contramap FromFaucet tracer)
        aliceChainConfig <- chainConfigFor Alice tmp cardanoClient [Carol] cperiod
        aliceChainContext <- loadChainContext aliceChainConfig alice [carol] hydraScriptsTxId

        withDirectChainTest (contramap (FromDirectChain "alice") tracer) aliceChainConfig aliceChainContext $
          \aliceChain@DirectChainTest{postTx = alicePostTx} -> do
            -- Bob setup
            bobChainConfig <- chainConfigFor Bob tmp cardanoClient [Alice, Carol] cperiod
            bobChainContext <- loadChainContext bobChainConfig bob [alice, carol] hydraScriptsTxId

            withDirectChainTest nullTracer bobChainConfig bobChainContext $
              \DirectChainTest{postTx = bobPostTx} -> do
                -- Scenario
                alicePostTx $ InitTx $ HeadParameters cperiod [alice, carol]
                aliceChain `observesInTimeSatisfying` hasInitTxWith cperiod [alice, carol]

                bobPostTx (AbortTx mempty)
                  `shouldThrow` \case
                    InvalidStateToPost{txTried} -> txTried == AbortTx @Tx mempty
                    _ -> False

  it "can commit" $ \tracer ->
    withTempDir "hydra-cluster" $ \tmp -> do
      withCardanoNodeDevnet (contramap FromNode tracer) tmp $ \cardanoClient -> do
        hydraScriptsTxId <- publishHydraScriptsAs cardanoClient Faucet
        -- Alice setup
        (aliceCardanoVk, aliceCardanoSk) <- keysFor Alice
        seedFromFaucet_ cardanoClient aliceCardanoVk 100_000_000 (contramap FromFaucet tracer)
        aliceChainConfig <- chainConfigFor Alice tmp cardanoClient [] cperiod
        aliceChainContext <- loadChainContext aliceChainConfig alice [] hydraScriptsTxId
        withDirectChainTest (contramap (FromDirectChain "alice") tracer) aliceChainConfig aliceChainContext $
          \aliceChain@DirectChainTest{postTx} -> do
            aliceUTxO <- seedFromFaucet cardanoClient aliceCardanoVk 1_000_000 (contramap FromFaucet tracer)

            postTx $ InitTx $ HeadParameters cperiod [alice]

            aliceChain `observesInTimeSatisfying` hasInitTxWith cperiod [alice]
            -- deliberately use alice's key known to hydra-node to trigger the error
            externalCommit cardanoClient aliceChain aliceCardanoSk aliceUTxO
              `shouldThrow` \case
                (SpendingNodeUtxoForbidden :: PostTxError Tx) -> True
                _ -> False

            (aliceExternalVk, aliceExternalSk) <- generate genKeyPair
            newAliceUTxO <- seedFromFaucet cardanoClient aliceExternalVk 1_000_000 (contramap FromFaucet tracer)

            externalCommit cardanoClient aliceChain aliceExternalSk newAliceUTxO
            aliceChain `observesInTime` OnCommitTx alice newAliceUTxO

  it "can commit empty UTxO" $ \tracer -> do
    withTempDir "hydra-cluster" $ \tmp -> do
      withCardanoNodeDevnet (contramap FromNode tracer) tmp $ \cardanoClient -> do
        hydraScriptsTxId <- publishHydraScriptsAs cardanoClient Faucet
        -- Alice setup
        (aliceCardanoVk, _) <- keysFor Alice
        seedFromFaucet_ cardanoClient aliceCardanoVk 100_000_000 (contramap FromFaucet tracer)
        aliceChainConfig <- chainConfigFor Alice tmp cardanoClient [] cperiod
        aliceChainContext <- loadChainContext aliceChainConfig alice [] hydraScriptsTxId
        withDirectChainTest (contramap (FromDirectChain "alice") tracer) aliceChainConfig aliceChainContext $
          \aliceChain@DirectChainTest{postTx} -> do
            -- Scenario
            postTx $ InitTx $ HeadParameters cperiod [alice]
            aliceChain `observesInTimeSatisfying` hasInitTxWith cperiod [alice]

            (_, aliceExternalSk) <- generate genKeyPair
            externalCommit cardanoClient aliceChain aliceExternalSk mempty
            aliceChain `observesInTime` OnCommitTx alice mempty

  it "can open, close & fanout a Head" $ \tracer -> do
    withTempDir "hydra-cluster" $ \tmp -> do
      withCardanoNodeDevnet (contramap FromNode tracer) tmp $ \cardanoClient-> do
        hydraScriptsTxId <- publishHydraScriptsAs cardanoClient Faucet
        -- Alice setup
        (aliceCardanoVk, _) <- keysFor Alice
        seedFromFaucet_ cardanoClient aliceCardanoVk 100_000_000 (contramap FromFaucet tracer)
        aliceChainConfig <- chainConfigFor Alice tmp cardanoClient [] cperiod
        aliceChainContext <- loadChainContext aliceChainConfig alice [] hydraScriptsTxId
        withDirectChainTest (contramap (FromDirectChain "alice") tracer) aliceChainConfig aliceChainContext $
          \aliceChain@DirectChainTest{postTx} -> do
            -- Scenario
            (aliceExternalVk, aliceExternalSk) <- generate genKeyPair
            someUTxO <- seedFromFaucet cardanoClient aliceExternalVk 1_000_000 (contramap FromFaucet tracer)

            postTx $ InitTx $ HeadParameters cperiod [alice]
            aliceChain `observesInTimeSatisfying` hasInitTxWith cperiod [alice]

            externalCommit cardanoClient aliceChain aliceExternalSk someUTxO
            aliceChain `observesInTime` OnCommitTx alice someUTxO

            postTx $ CollectComTx someUTxO
            aliceChain `observesInTime` OnCollectComTx

            let snapshot =
                  Snapshot
                    { number = 1
                    , utxo = someUTxO
                    , confirmed = []
                    }

            postTx . CloseTx $
              ConfirmedSnapshot
                { snapshot
                , signatures = aggregate [sign aliceSk snapshot]
                }

            deadline <-
              waitMatch aliceChain $ \case
                Observation{observedTx = OnCloseTx{snapshotNumber, contestationDeadline}}
                  | snapshotNumber == 1 -> Just contestationDeadline
                _ -> Nothing
            now <- getCurrentTime
            unless (deadline > now) $
              failure $
                "contestationDeadline in the past: " <> show deadline <> ", now: " <> show now
            delayUntil deadline

            waitMatch aliceChain $ \case
              Tick t _ | t > deadline -> Just ()
              _ -> Nothing
            postTx $
              FanoutTx
                { utxo = someUTxO
                , contestationDeadline = deadline
                }
            aliceChain `observesInTime` OnFanoutTx
            failAfter 5 $
              waitForUTxO cardanoClient someUTxO

  it "can restart head to point in the past and replay on-chain events" $ \tracer -> do
    withTempDir "direct-chain" $ \tmp -> do
      withCardanoNodeDevnet (contramap FromNode tracer) tmp $ \cardanoClient-> do
        hydraScriptsTxId <- publishHydraScriptsAs cardanoClient Faucet
        (aliceCardanoVk, _) <- keysFor Alice
        seedFromFaucet_ cardanoClient aliceCardanoVk 100_000_000 (contramap FromFaucet tracer)
        -- Alice setup
        aliceChainConfig <- chainConfigFor Alice tmp cardanoClient [] cperiod
        aliceChainContext <- loadChainContext aliceChainConfig alice [] hydraScriptsTxId

        -- Scenario
        tip <- withDirectChainTest (contramap (FromDirectChain "alice") tracer) aliceChainConfig aliceChainContext $
          \aliceChain@DirectChainTest{postTx} -> do
            tip <- queryTipClientOnline cardanoClient
            postTx $ InitTx $ HeadParameters cperiod [alice]
            aliceChain `observesInTimeSatisfying` hasInitTxWith cperiod [alice]
            pure tip

        let aliceChainConfig' = aliceChainConfig{startChainFrom = Just tip}
        -- REVIEW: It's a bit weird now that we would use the original chain
        -- state here. Does this test even make sense with persistence?
        withDirectChainTest (contramap (FromDirectChain "alice") tracer) aliceChainConfig' aliceChainContext $
          \aliceChain@DirectChainTest{} ->
            aliceChain `observesInTimeSatisfying` hasInitTxWith cperiod [alice]

  it "cannot restart head to an unknown point" $ \tracer -> do
    withTempDir "direct-chain" $ \tmp -> do
      withCardanoNodeDevnet (contramap FromNode tracer) tmp $ \cardanoClient -> do
        (aliceCardanoVk, _) <- keysFor Alice
        seedFromFaucet_ cardanoClient aliceCardanoVk 100_000_000 (contramap FromFaucet tracer)
        hydraScriptsTxId <- publishHydraScriptsAs cardanoClient Faucet

        let headerHash = fromString (replicate 64 '0')
        let fakeTip = ChainPoint 42 headerHash
        aliceChainConfig <-
          chainConfigFor Alice tmp cardanoClient [] cperiod
            <&> \cfg -> cfg{startChainFrom = Just fakeTip}
        aliceChainContext <- loadChainContext aliceChainConfig alice [] hydraScriptsTxId
        let action = withDirectChainTest (contramap (FromDirectChain "alice") tracer) aliceChainConfig aliceChainContext $ \_ ->
              threadDelay 5 >> fail "should not execute main action but did?"

        action `shouldThrow` \case
          IntersectionNotFound{} -> True

  it "can publish and query reference scripts in a timely manner" $ \tracer -> do
    withTempDir "direct-chain" $ \tmp -> do
      withCardanoNodeDevnet (contramap FromNode tracer) tmp $ \cardanoClient -> do
        DirectChainConfig{cardanoSigningKey} <- chainConfigFor Faucet tmp cardanoClient [] cperiod
        hydraScriptsTxIdStr <-
          readCreateProcess
            ( proc
                "hydra-node"
                ( "publish-scripts"
                    : mconcat
                      [ ["--node-socket", unFile $ nodeSocketClientOnline cardanoClient]
                      , toArgNetworkId $ networkIdClientOnline cardanoClient
                      , ["--cardano-signing-key", cardanoSigningKey]
                      ]
                )
            )
            ""
        let hydraScriptsTxId = fromString hydraScriptsTxIdStr
        failAfter 5 $ void $ queryScriptRegistry cardanoClient hydraScriptsTxId

  it "can only contest once" $ \tracer -> do
    withTempDir "hydra-cluster" $ \tmp -> do
      withCardanoNodeDevnet (contramap FromNode tracer) tmp $ \cardanoClient -> do
        hydraScriptsTxId <- publishHydraScriptsAs cardanoClient Faucet
        -- Alice setup
        (aliceCardanoVk, _) <- keysFor Alice
        seedFromFaucet_ cardanoClient aliceCardanoVk 100_000_000 (contramap FromFaucet tracer)
        aliceChainConfig <- chainConfigFor Alice tmp cardanoClient [] cperiod
        aliceChainContext <- loadChainContext aliceChainConfig alice [] hydraScriptsTxId
        withDirectChainTest (contramap (FromDirectChain "alice") tracer) aliceChainConfig aliceChainContext $
          \aliceChain@DirectChainTest{postTx} -> do
            (aliceExternalVk, aliceExternalSk) <- generate genKeyPair
            someUTxO <- seedFromFaucet cardanoClient aliceExternalVk 1_000_000 (contramap FromFaucet tracer)

            postTx $ InitTx $ HeadParameters cperiod [alice]
            aliceChain `observesInTimeSatisfying` hasInitTxWith cperiod [alice]

            externalCommit cardanoClient aliceChain aliceExternalSk someUTxO
            aliceChain `observesInTime` OnCommitTx alice someUTxO

            postTx $ CollectComTx someUTxO
            aliceChain `observesInTime` OnCollectComTx
            -- Head is open with someUTxO

            -- Alice close with the initial snapshot U0
            postTx $ CloseTx InitialSnapshot{initialUTxO = someUTxO}
            waitMatch aliceChain $ \case
              Observation{observedTx = OnCloseTx{snapshotNumber}}
                | snapshotNumber == 0 -> Just ()
              _ -> Nothing

            -- Alice contests with some snapshot U1 -> successful
            let snapshot1 =
                  Snapshot
                    { number = 1
                    , utxo = someUTxO
                    , confirmed = []
                    }
            postTx . ContestTx $
              ConfirmedSnapshot
                { snapshot = snapshot1
                , signatures = aggregate [sign aliceSk snapshot1]
                }
            aliceChain `observesInTime` OnContestTx{snapshotNumber = 1}

            -- Alice contests with some snapshot U2 -> expect fail
            let snapshot2 =
                  Snapshot
                    { number = 2
                    , utxo = someUTxO
                    , confirmed = []
                    }
            let contestAgain =
                  postTx . ContestTx $
                    ConfirmedSnapshot
                      { snapshot = snapshot2
                      , signatures = aggregate [sign aliceSk snapshot2]
                      }
            -- NOTE: We deliberately expect the transaction creation and
            -- submission code of the Chain.Direct module to fail here because
            -- the scripts don't validate. That is, the on-chain code prevented
            -- this from happening and NOT any off-chain guard we added. Also
            -- note, that we don't try to check whether it's failing for the
            -- right reason here (see corresponding mutation test for this).
            contestAgain `shouldThrow` \case
              (ScriptFailedInWallet{} :: PostTxError Tx) -> True
              _ -> False

data DirectChainTestLog
  = FromNode NodeLog
  | FromDirectChain Text DirectChainLog
  | FromFaucet FaucetLog
  deriving (Show, Generic, ToJSON)

data DirectChainTest tx m = DirectChainTest
  { postTx :: PostChainTx tx -> m ()
  , waitCallback :: m (ChainEvent tx)
  , draftCommitTx :: UTxO' (TxOut CtxUTxO, Witness WitCtxTxIn) -> m tx
  }

-- | Wrapper around 'withDirectChain' that threads a 'ChainStateType tx' through
-- 'postTx' and 'waitCallback' calls.
withDirectChainTest ::
  Tracer IO DirectChainLog ->
  ChainConfig ->
  ChainContext ->
  (DirectChainTest Tx IO -> IO a) ->
  IO a
withDirectChainTest tracer config ctx action = do
  eventMVar <- newEmptyTMVarIO

  let callback event = atomically $ putTMVar eventMVar event

  wallet <- mkTinyWallet tracer config

  withDirectChain tracer config ctx wallet (initHistory initialChainState) callback $ \Chain{postTx, draftCommitTx} -> do
    action
      DirectChainTest
        { postTx
        , waitCallback = atomically $ takeTMVar eventMVar
        , draftCommitTx = \utxo -> do
            eTx <- draftCommitTx utxo
            case eTx of
              Left e -> throwIO e
              Right tx -> pure tx
        }

hasInitTxWith :: (HasCallStack, IsTx tx) => ContestationPeriod -> [Party] -> OnChainTx tx -> Expectation
hasInitTxWith expectedContestationPeriod expectedParties = \case
  OnInitTx{contestationPeriod, parties} -> do
    expectedContestationPeriod `shouldBe` contestationPeriod
    expectedParties `shouldBe` parties
  tx -> failure ("Unexpected observation: " <> show tx)

observesInTime :: IsTx tx => DirectChainTest tx IO -> OnChainTx tx -> IO ()
observesInTime chain expected =
  observesInTimeSatisfying chain (`shouldBe` expected)

observesInTimeSatisfying :: DirectChainTest tx IO -> (OnChainTx tx -> Expectation) -> IO ()
observesInTimeSatisfying DirectChainTest{waitCallback} check =
  failAfter 10 go
 where
  go = do
    e <- waitCallback
    case e of
      Observation{observedTx} ->
        check observedTx
      _TickOrRollback ->
        go

waitMatch :: DirectChainTest tx IO -> (ChainEvent tx -> Maybe b) -> IO b
waitMatch DirectChainTest{waitCallback} match = go
 where
  go = do
    a <- waitCallback
    maybe go pure (match a)

delayUntil :: (MonadDelay m, MonadTime m) => UTCTime -> m ()
delayUntil target = do
  now <- getCurrentTime
  threadDelay . realToFrac $ diffUTCTime target now

-- Commit using a wallet/external unknown to a hydra-node.
externalCommit ::
  RunningNode ->
  DirectChainTest Tx IO ->
  SigningKey PaymentKey ->
  UTxO' (TxOut CtxUTxO) ->
  IO ()
externalCommit node hydraClient externalSk utxoToCommit' = do
  let utxoToCommit =
        UTxO $ (,KeyWitness KeyWitnessForSpending) <$> toMap utxoToCommit'

  commitTx <- draftCommitTx utxoToCommit
  let signedTx = signTx externalSk commitTx
  submitTx node signedTx
 where
  DirectChainTest{draftCommitTx} = hydraClient
