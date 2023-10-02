{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds #-}
-- | A basic cardano-node client that can talk to a local cardano-node.
--
-- The idea of this module is to provide a Haskell interface on top of
-- cardano-cli's API, using cardano-api types.
module Hydra.Chain.CardanoClient (
  -- * CardanoClient handle
  IsCardanoClient (..),
  CardanoClient (..),
  ClientMode (..),
  modeTag,
  mkCardanoClientOnline,

  -- * Querying
  QueryType (..),

  -- * Tx Construction / Submission
  buildTransaction,
  submitTransaction,
  SubmitTransactionException (..),
  awaitTransaction,
  -- * Local state query
  QueryPoint (..),
  -- queryTip,
  queryTipSlotNo,
  querySystemStart,
  queryEraHistory,
  queryProtocolParameters,
  queryGenesisParameters,
  queryUTxO,
  queryUTxOByTxIn,
  queryUTxOWhole,
  queryUTxOFor,
  queryStakePools,
  -- * Helpers
  runQuery,
  throwOnEraMismatch,
  localNodeConnectInfo,
  cardanoModeParams,
  -- * Exceptions
  QueryException (..),
  ) where

import Hydra.Prelude

import Hydra.Cardano.Api hiding (Block, queryGenesisParameters)

import qualified Cardano.Api.UTxO as UTxO
import Cardano.Ledger.Core (PParams)
import qualified Data.Set as Set
import Ouroboros.Consensus.HardFork.Combinator.AcrossEras (EraMismatch)
import Test.QuickCheck (oneof)
import Hydra.Ledger (IsTx)
import Hydra.Ledger.Cardano ()

data QueryException
  = QueryAcquireException AcquiringFailure
  | QueryEraMismatchException EraMismatch
  | QueryProtocolParamsConversionException ProtocolParametersConversionError
  deriving (Show)

instance Eq QueryException where
  a == b = case (a, b) of
    (QueryAcquireException af1, QueryAcquireException af2) -> case (af1, af2) of
      (AFPointTooOld, AFPointTooOld) -> True
      (AFPointNotOnChain, AFPointNotOnChain) -> True
      _ -> False
    (QueryEraMismatchException em1, QueryEraMismatchException em2) -> em1 == em2
    _ -> False

instance Exception QueryException

-- * CardanoClient handle

-- draft with type families (+ gadt)
-- this can be opened up by removing the equality constraint with the CardanoClient GADT

--TODO(Elaine): QueryAt constructor stopped being used in 8967f3544f3d9c3a1566ee4fe0d7d2d003c1349e as part of #621
-- so we probably don't need this in the first place, but it's included to make offline mode (which only queries tip) not blocked on removal
class QueryType q where
  queryTypeTip :: q

instance QueryType QueryPoint where
  queryTypeTip = QueryTip

class
  ( IsTx (TxType mode)
  , QueryType (QueryType' mode)
  , Typeable mode
  , CardanoClientType mode ~ CardanoClient mode
  ) => IsCardanoClient (mode :: ClientMode) where
  type CardanoClientType mode = (r :: Type) | r -> mode
  type CardanoClientType mode = CardanoClient mode

  --TODO(ELAINE): do we want transaction type to always be fixed for a given mode?
  -- id assume so, i cant think of a use for onlinemode + non Tx, seems like itd add complexity
  -- okay after thinking about how this is getting used in the main executable, id say yes, we do want it to be fixed
  type TxType mode :: Type

  -- QueryPoint generalization
  -- planned so that offlinemode can focus support on querying tip
  type QueryType' mode :: Type


  -- these are all the methods that are common to both online and offline mode
  --TODO(ELAINE): rename this, but this should be how to call the methods on the client
  submitTransactionClient :: CardanoClientType mode -> TxType mode -> IO ()

  queryTipClient :: CardanoClientType mode -> IO ChainPoint
  querySystemStartClient :: CardanoClientType mode -> QueryType' mode -> IO SystemStart
  queryEraHistoryClient :: CardanoClientType mode -> QueryType' mode -> IO (EraHistory CardanoMode)
  queryProtocolParametersClient :: CardanoClientType mode -> QueryType' mode -> IO (PParams LedgerEra)
  queryGenesisParametersClient :: CardanoClientType mode -> QueryType' mode -> IO (GenesisParameters ShelleyEra)


--draft with data families (precluding gadt)
-- class (IsTx (TxType mode) ) => IsCardanoClient (mode :: ClientMode) where
--   data CardanoClient mode :: Type
--   --TODO(ELAINE): do we want transaction type to always be fixed for a given mode?
--   -- id assume so, i cant think of a use for onlinemode + non Tx, seems like itd add complexity
--   -- okay after thinking about how this is getting used in the main executable, id say yes, we do want it to be fixed
--   type TxType mode :: Type

--   submitTransactionClass :: CardanoClient mode -> TxType mode -> IO ()
--   -- queryTipClass :: CardanoClient mode -> IO ChainTip

-- instance IsCardanoClient 'ClientOffline where
--   type TxType 'ClientOffline = Tx

--   submitTransactionClass = undefined
--   data CardanoClient 'ClientOffline = CardanoClientOffline
--     {}

-- instance IsCardanoClient 'ClientOnline where
--   type TxType 'ClientOnline = Tx

--   submitTransactionClass = submitTransactionNEW
--   data CardanoClient 'ClientOnline = CardanoClientOnline
--     {
--       --TODO(ELAINE): rename these
--       buildTransactionNEW :: AddressInEra -> UTxO -> [TxIn] -> [TxOut CtxTx] -> IO (Either TxBodyErrorAutoBalance TxBody)
--     , submitTransactionNEW :: Tx -> IO ()
--     , awaitTransactionNEW :: Tx -> IO UTxO
--     , queryTipNEW :: IO ChainTip -- TODO(ELAINE): would it be possible to dedup this with that record that queries for current slot etc
--     -- , queryGlobals :: IO Globals
--     , querySystemStartNEW :: QueryPoint -> IO SystemStart
--     , queryEraHistoryNEW :: QueryPoint -> IO (EraHistory CardanoMode)
--     -- , queryProtocolParametersNEW :: QueryPoint -> IO BundledProtocolParameters
--     -- , queryGenesisParametersNEW :: QueryPoint -> IO GenesisParameters
--     , queryUTxONEW :: QueryPoint -> [Address ShelleyAddr] -> IO UTxO
--     , queryUTxOByTxInNEW :: QueryPoint -> [TxIn] -> IO UTxO
--     , queryUTxOWholeNEW :: QueryPoint -> IO UTxO
--     , queryUTxOForNEW :: QueryPoint -> VerificationKey PaymentKey -> IO UTxO
--     , queryStakePoolsNEW :: QueryPoint -> IO (Set PoolId)
--     , networkIdNEW :: NetworkId

--     , queryUTxOByAddress :: [Address ShelleyAddr] -> IO UTxO
--     , networkId :: NetworkId
--     }
  -- mkCardanoClient = mkCardanoClientOnline
-- thing :: IsCardanoClient mode => CardanoClient mode
-- thing = mkCardanoClient undefined undefined 

-- thing from gadt
-- thing :: IsCardanoClient (mode :: ClientMode) => CardanoClient mode -> TxType mode -> IO ()
-- thing = submitTransactionClass


-- pattern match using type level proof
-- thingOnline :: CardanoClient mode -> CardanoClient mode -> TxType mode -> IO ()
-- thingOnline c = case c of
--   CardanoClientOnline{} -> submitTransactionNEW
--   CardanoClientOffline{} -> undefined



-- onlyOnlineCode 

-- defined with datakinds instead of an open typeclass, because we probably don't actually want people to extend this willy nilly
data ClientMode = ClientOnline | ClientOffline
  deriving (Eq, Show, Generic, Typeable)


--GADT:
data CardanoClient (mode :: ClientMode) where
  CardanoClientOffline ::
    {}  -> CardanoClient 'ClientOffline
  CardanoClientOnline ::
    {
        buildTransactionClientOnline :: AddressInEra -> UTxO -> [TxIn] -> [TxOut CtxTx] -> IO (Either TxBodyErrorAutoBalance TxBody)
      , submitTransactionClientOnline :: Tx -> IO ()
      , awaitTransactionClientOnline :: Tx -> IO UTxO
      , queryTipClientOnline :: IO ChainPoint -- TODO(ELAINE): would it be possible to dedup this with that record that queries for current slot etc
      -- , queryGlobals :: IO Globals
      , querySystemStartClientOnline :: QueryPoint -> IO SystemStart
      , queryEraHistoryClientOnline :: QueryPoint -> IO (EraHistory CardanoMode)
      , queryProtocolParametersClientOnline :: QueryPoint -> IO (PParams LedgerEra)
      , queryGenesisParametersClientOnline :: QueryPoint -> IO (GenesisParameters ShelleyEra)
      , queryUTxOClientOnline :: QueryPoint -> [Address ShelleyAddr] -> IO UTxO
      , queryUTxOByTxInClientOnline :: QueryPoint -> [TxIn] -> IO UTxO
      , queryUTxOWholeClientOnline :: QueryPoint -> IO UTxO
      , queryUTxOForClientOnline :: QueryPoint -> VerificationKey PaymentKey -> IO UTxO
      , queryStakePoolsClientOnline :: QueryPoint -> IO (Set PoolId)
      
      , networkIdClientOnline :: NetworkId
      , nodeSocketClientOnline :: SocketPath
      
      , queryUTxOByAddressClientOnline :: [Address ShelleyAddr] -> IO UTxO
    
    } -> CardanoClient 'ClientOnline

mkCardanoClientOnline :: NetworkId -> SocketPath -> CardanoClient 'ClientOnline
mkCardanoClientOnline networkId nodeSocket =
  CardanoClientOnline
    {
      buildTransactionClientOnline = buildTransaction networkId nodeSocket
    , submitTransactionClientOnline = submitTransaction networkId nodeSocket
    , awaitTransactionClientOnline = awaitTransaction networkId nodeSocket
    -- | NOTE: different type from the old one
    , queryTipClientOnline = queryTip networkId nodeSocket-- getLocalChainTip (localNodeConnectInfo networkId nodeSocket)
    -- , queryGlobals = HydraLedger.newGlobals =<< queryGenesisParameters networkId nodeSocket QueryTip
    , querySystemStartClientOnline = querySystemStart networkId nodeSocket
    , queryEraHistoryClientOnline = queryEraHistory networkId nodeSocket
    , queryProtocolParametersClientOnline = queryProtocolParameters networkId nodeSocket
    , queryGenesisParametersClientOnline = queryGenesisParameters networkId nodeSocket    
    , queryUTxOClientOnline = queryUTxO networkId nodeSocket
    , queryUTxOByTxInClientOnline = queryUTxOByTxIn networkId nodeSocket
    , queryUTxOWholeClientOnline = queryUTxOWhole networkId nodeSocket
    , queryUTxOForClientOnline = queryUTxOFor networkId nodeSocket
    , queryStakePoolsClientOnline = queryStakePools networkId nodeSocket

    , networkIdClientOnline = networkId
    , nodeSocketClientOnline = nodeSocket
    
    , queryUTxOByAddressClientOnline = queryUTxO networkId nodeSocket QueryTip

    
    }


instance IsCardanoClient 'ClientOnline where
  type TxType 'ClientOnline = Tx
  type QueryType' 'ClientOnline = QueryPoint

  submitTransactionClient = submitTransactionClientOnline
  queryTipClient = queryTipClientOnline
  querySystemStartClient = querySystemStartClientOnline
  queryEraHistoryClient = queryEraHistoryClientOnline
  queryProtocolParametersClient = queryProtocolParametersClientOnline
  queryGenesisParametersClient = queryGenesisParametersClientOnline

--TODO(ELAINE): is this enough reflection to make stuff convenient?
modeTag :: CardanoClient (mode :: ClientMode) -> ClientMode
modeTag c = case c of
  CardanoClientOffline{} -> ClientOffline
  CardanoClientOnline{} -> ClientOnline

-- typeclass for checking if a mode is online
-- can be opened up for multiple variants of online mode by removing the equality constraint
-- class (mode ~ 'ClientOnline) => IsCardanoClientOnline (mode :: ClientMode) where
  
-- instance IsCardanoClientOnline 'ClientOnline where

-- data CardanoClient (mode :: ClientMode) where
--   CardanoClientOnline ::
--     {
--       --TODO(ELAINE): rename these
--       buildTransactionNEW :: AddressInEra -> UTxO -> [TxIn] -> [TxOut CtxTx] -> IO (Either TxBodyErrorAutoBalance TxBody)
--     , submitTransactionNEW :: Tx -> IO ()
--     , awaitTransactionNEW :: Tx -> IO UTxO
--     , queryTipNEW :: IO ChainTip -- TODO(ELAINE): would it be possible to dedup this with that record that queries for current slot etc
--     -- , queryGlobals :: IO Globals
--     , querySystemStartNEW :: QueryPoint -> IO SystemStart
--     , queryEraHistoryNEW :: QueryPoint -> IO (EraHistory CardanoMode)
--     -- , queryProtocolParametersNEW :: QueryPoint -> IO BundledProtocolParameters
--     -- , queryGenesisParametersNEW :: QueryPoint -> IO GenesisParameters
--     , queryUTxONEW :: QueryPoint -> [Address ShelleyAddr] -> IO UTxO
--     , queryUTxOByTxInNEW :: QueryPoint -> [TxIn] -> IO UTxO
--     , queryUTxOWholeNEW :: QueryPoint -> IO UTxO
--     , queryUTxOForNEW :: QueryPoint -> VerificationKey PaymentKey -> IO UTxO
--     , queryStakePoolsNEW :: QueryPoint -> IO (Set PoolId)
--     , networkIdNEW :: NetworkId

--     , queryUTxOByAddress :: [Address ShelleyAddr] -> IO UTxO
--     , networkId :: NetworkId
--     }  -> CardanoClient 'ClientOnline

-- | Handle interface for abstract querying of a cardano node.
-- data CardanoClient = CardanoClient
--   { queryUTxOByAddress :: [Address ShelleyAddr] -> IO UTxO
--   , networkId :: NetworkId
--   }

-- | Construct a 'CardanoClient' handle.
-- mkCardanoClient :: NetworkId -> SocketPath -> CardanoClient
-- mkCardanoClient networkId nodeSocket =
--   CardanoClient
--     { queryUTxOByAddress = queryUTxO networkId nodeSocket QueryTip
--     , networkId
--     }

-- | Construct a 'CardanoClient' handle.
-- mkCardanoClientOnline :: NetworkId -> SocketPath -> CardanoClient 'ClientOnline
-- mkCardanoClientOnline networkId nodeSocket =
--   CardanoClientOnline
--     {
--       buildTransactionNEW = buildTransaction networkId nodeSocket
--     , submitTransactionNEW = submitTransaction networkId nodeSocket
--     , awaitTransactionNEW = awaitTransaction networkId nodeSocket
--     -- | NOTE: different type from the old one
--     , queryTipNEW = getLocalChainTip (localNodeConnectInfo networkId nodeSocket)
--     -- , queryGlobals = HydraLedger.newGlobals =<< queryGenesisParameters networkId nodeSocket QueryTip
--     , querySystemStartNEW = querySystemStart networkId nodeSocket
--     , queryEraHistoryNEW = queryEraHistory networkId nodeSocket
--     -- , queryProtocolParametersNEW = queryProtocolParameters networkId nodeSocket
--     -- , queryGenesisParametersNEW = queryGenesisParameters networkId nodeSocket
--     , queryUTxONEW = queryUTxO networkId nodeSocket
--     , queryUTxOByTxInNEW = queryUTxOByTxIn networkId nodeSocket
--     , queryUTxOWholeNEW = queryUTxOWhole networkId nodeSocket
--     , queryUTxOForNEW = queryUTxOFor networkId nodeSocket
--     , queryStakePoolsNEW = queryStakePools networkId nodeSocket
--     , networkIdNEW = networkId
--     , queryUTxOByAddress = queryUTxO networkId nodeSocket QueryTip
--     , networkId
--     }

-- * Tx Construction / Submission

-- | Construct a simple payment consuming some inputs and producing some
-- outputs (no certificates or withdrawals involved).
--
-- On success, the returned transaction is fully balanced. On error, return
-- `TxBodyErrorAutoBalance`.
buildTransaction ::
  -- | Current network identifier
  NetworkId ->
  -- | Filepath to the cardano-node's domain socket
  SocketPath ->
  -- | Change address to send
  AddressInEra ->
  -- | Unspent transaction outputs to spend.
  UTxO ->
  -- | Collateral inputs.
  [TxIn] ->
  -- | Outputs to create.
  [TxOut CtxTx] ->
  IO (Either TxBodyErrorAutoBalance TxBody)
buildTransaction networkId socket changeAddress utxoToSpend collateral outs = do
  pparams <- queryProtocolParameters networkId socket QueryTip
  systemStart <- querySystemStart networkId socket QueryTip
  eraHistory <- queryEraHistory networkId socket QueryTip
  stakePools <- queryStakePools networkId socket QueryTip
  pure $
    second balancedTxBody $
      makeTransactionBodyAutoBalance
        systemStart
        (toLedgerEpochInfo eraHistory)
        (LedgerProtocolParameters pparams)
        stakePools
        mempty
        mempty
        (UTxO.toApi utxoToSpend)
        (bodyContent pparams)
        changeAddress
        Nothing
 where
  -- NOTE: 'makeTransactionBodyAutoBalance' overwrites this.
  dummyFeeForBalancing = TxFeeExplicit 0

  bodyContent pparams =
    TxBodyContent
      (withWitness <$> toList (UTxO.inputSet utxoToSpend))
      (TxInsCollateral collateral)
      TxInsReferenceNone
      outs
      TxTotalCollateralNone
      TxReturnCollateralNone
      dummyFeeForBalancing
      (TxValidityNoLowerBound, TxValidityNoUpperBound)
      TxMetadataNone
      TxAuxScriptsNone
      TxExtraKeyWitnessesNone
      (BuildTxWith $ Just $ LedgerProtocolParameters pparams)
      TxWithdrawalsNone
      TxCertificatesNone
      TxUpdateProposalNone
      TxMintValueNone
      TxScriptValidityNone
      Nothing
      Nothing

-- | Submit a (signed) transaction to the node.
--
-- Throws 'SubmitTransactionException' if submission fails.
submitTransaction ::
  -- | Current network discriminant
  NetworkId ->
  -- | Filepath to the cardano-node's domain socket
  SocketPath ->
  -- | A signed transaction.
  Tx ->
  IO ()
submitTransaction networkId socket tx =
  submitTxToNodeLocal (localNodeConnectInfo networkId socket) txInMode >>= \case
    SubmitSuccess ->
      pure ()
    SubmitFail (TxValidationEraMismatch e) ->
      throwIO (SubmitEraMismatch e)
    SubmitFail e@TxValidationErrorInMode{} ->
      throwIO (SubmitTxValidationError e)
 where
  txInMode =
    TxInMode tx BabbageEraInCardanoMode

-- | Exceptions that 'can' occur during a transaction submission.
--
-- In principle, we can only encounter an 'EraMismatch' at era boundaries, when
-- we try to submit a "next era" transaction as a "current era" transaction, or
-- vice-versa.
-- Similarly, 'TxValidationError' shouldn't occur given that the transaction was
-- safely constructed through 'buildTransaction'.
data SubmitTransactionException
  = SubmitEraMismatch EraMismatch
  | SubmitTxValidationError (TxValidationErrorInMode CardanoMode)
  deriving (Show)

instance Exception SubmitTransactionException

-- | Await until the given transaction is visible on-chain. Returns the UTxO
-- set produced by that transaction.
--
-- Note that this function loops forever; hence, one probably wants to couple it
-- with a surrounding timeout.
awaitTransaction ::
  -- | Current network discriminant
  NetworkId ->
  -- | Filepath to the cardano-node's domain socket
  SocketPath ->
  -- | The transaction to watch / await
  Tx ->
  IO UTxO
awaitTransaction networkId socket tx =
  go
 where
  ins = keys (UTxO.toMap $ utxoFromTx tx)
  go = do
    utxo <- queryUTxOByTxIn networkId socket QueryTip ins
    if null utxo
      then go
      else pure utxo

-- * Local state query

-- | Describes whether to query at the tip or at a specific point.
data QueryPoint = QueryTip | QueryAt ChainPoint
  deriving (Eq, Show, Generic)

deriving instance ToJSON QueryPoint

instance Arbitrary QueryPoint where
  -- XXX: This is not complete as we lack an 'Arbitrary ChainPoint' and we have
  -- not bothered about it yet.
  arbitrary =
    oneof
      [ pure QueryTip
      , pure $ QueryAt ChainPointAtGenesis
      ]

-- | Query the latest chain point aka "the tip".
queryTip :: NetworkId -> SocketPath -> IO ChainPoint
queryTip networkId socket =
  chainTipToChainPoint <$> getLocalChainTip (localNodeConnectInfo networkId socket)

-- | Query the latest chain point just for the slot number.
queryTipSlotNo :: IsCardanoClient mode => CardanoClient mode -> IO SlotNo
queryTipSlotNo cardanoClient =
  queryTipClient cardanoClient >>= \case
    ChainPointAtGenesis -> pure 0
    ChainPoint slotNo _ -> pure slotNo
-- queryTipSlotNo :: NetworkId -> SocketPath -> IO SlotNo
-- queryTipSlotNo networkId socket =
--   getLocalChainTip (localNodeConnectInfo networkId socket) >>= \case
--     ChainTipAtGenesis -> pure 0
--     ChainTip slotNo _ _ -> pure slotNo

-- | Query the system start parameter at given point.
--
-- Throws at least 'QueryException' if query fails.
querySystemStart :: NetworkId -> SocketPath -> QueryPoint -> IO SystemStart
querySystemStart networkId socket queryPoint =
  runQuery networkId socket queryPoint QuerySystemStart

-- | Query the era history at given point.
--
-- Throws at least 'QueryException' if query fails.
queryEraHistory :: NetworkId -> SocketPath -> QueryPoint -> IO (EraHistory CardanoMode)
queryEraHistory networkId socket queryPoint =
  runQuery networkId socket queryPoint $ QueryEraHistory CardanoModeIsMultiEra

-- | Query the protocol parameters at given point.
--
-- Throws at least 'QueryException' if query fails.
queryProtocolParameters ::
  NetworkId ->
  SocketPath ->
  QueryPoint ->
  IO (PParams LedgerEra)
queryProtocolParameters networkId socket queryPoint = do
  let query =
        QueryInEra
          BabbageEraInCardanoMode
          ( QueryInShelleyBasedEra
              ShelleyBasedEraBabbage
              QueryProtocolParameters
          )
  runQuery networkId socket queryPoint query >>= throwOnEraMismatch

-- | Query 'GenesisParameters' at a given point.
--
-- Throws at least 'QueryException' if query fails.
queryGenesisParameters :: NetworkId -> SocketPath -> QueryPoint -> IO (GenesisParameters ShelleyEra)
queryGenesisParameters networkId socket queryPoint =
  let query =
        QueryInEra
          BabbageEraInCardanoMode
          ( QueryInShelleyBasedEra
              ShelleyBasedEraBabbage
              QueryGenesisParameters
          )
   in runQuery networkId socket queryPoint query >>= throwOnEraMismatch

-- | Query UTxO for all given addresses at given point.
--
-- Throws at least 'QueryException' if query fails.
queryUTxO :: NetworkId -> SocketPath -> QueryPoint -> [Address ShelleyAddr] -> IO UTxO
queryUTxO networkId socket queryPoint addresses =
  let query =
        QueryInEra
          BabbageEraInCardanoMode
          ( QueryInShelleyBasedEra
              ShelleyBasedEraBabbage
              ( QueryUTxO
                  (QueryUTxOByAddress (Set.fromList $ map AddressShelley addresses))
              )
          )
   in UTxO.fromApi <$> (runQuery networkId socket queryPoint query >>= throwOnEraMismatch)

-- | Query UTxO for given tx inputs at given point.
--
-- Throws at least 'QueryException' if query fails.
queryUTxOByTxIn :: NetworkId -> SocketPath -> QueryPoint -> [TxIn] -> IO UTxO
queryUTxOByTxIn networkId socket queryPoint inputs =
  let query =
        QueryInEra
          BabbageEraInCardanoMode
          ( QueryInShelleyBasedEra
              ShelleyBasedEraBabbage
              (QueryUTxO (QueryUTxOByTxIn (Set.fromList inputs)))
          )
   in UTxO.fromApi <$> (runQuery networkId socket queryPoint query >>= throwOnEraMismatch)

-- | Query the whole UTxO from node at given point. Useful for debugging, but
-- should obviously not be used in production code.
--
-- Throws at least 'QueryException' if query fails.
queryUTxOWhole :: NetworkId -> SocketPath -> QueryPoint -> IO UTxO
queryUTxOWhole networkId socket queryPoint = do
  UTxO.fromApi <$> (runQuery networkId socket queryPoint query >>= throwOnEraMismatch)
 where
  query =
    QueryInEra
      BabbageEraInCardanoMode
      ( QueryInShelleyBasedEra
          ShelleyBasedEraBabbage
          (QueryUTxO QueryUTxOWhole)
      )

-- | Query UTxO for the address of given verification key at point.
--
-- Throws at least 'QueryException' if query fails.
queryUTxOFor :: NetworkId -> SocketPath -> QueryPoint -> VerificationKey PaymentKey -> IO UTxO
queryUTxOFor networkId nodeSocket queryPoint vk =
  case mkVkAddress networkId vk of
    ShelleyAddressInEra addr ->
      queryUTxO networkId nodeSocket queryPoint [addr]
    ByronAddressInEra{} ->
      fail "impossible: mkVkAddress returned Byron address."

-- | Query the current set of registered stake pools.
--
-- Throws at least 'QueryException' if query fails.
queryStakePools :: NetworkId -> SocketPath -> QueryPoint -> IO (Set PoolId)
queryStakePools networkId socket queryPoint =
  let query =
        QueryInEra
          BabbageEraInCardanoMode
          ( QueryInShelleyBasedEra
              ShelleyBasedEraBabbage
              QueryStakePools
          )
   in runQuery networkId socket queryPoint query >>= throwOnEraMismatch

-- | Throws at least 'QueryException' if query fails.
runQuery :: NetworkId -> SocketPath -> QueryPoint -> QueryInMode CardanoMode a -> IO a
runQuery networkId socket point query =
  queryNodeLocalState (localNodeConnectInfo networkId socket) maybePoint query >>= \case
    Left err -> throwIO $ QueryAcquireException err
    Right result -> pure result
 where
  maybePoint =
    case point of
      QueryTip -> Nothing
      QueryAt cp -> Just cp

-- * Helpers

throwOnEraMismatch :: (MonadThrow m) => Either EraMismatch a -> m a
throwOnEraMismatch res =
  case res of
    Left eraMismatch -> throwIO $ QueryEraMismatchException eraMismatch
    Right result -> pure result

localNodeConnectInfo :: NetworkId -> SocketPath -> LocalNodeConnectInfo CardanoMode
localNodeConnectInfo = LocalNodeConnectInfo cardanoModeParams

cardanoModeParams :: ConsensusModeParams CardanoMode
cardanoModeParams = CardanoModeParams $ EpochSlots defaultByronEpochSlots
 where
  -- NOTE(AB): extracted from Parsers in cardano-cli, this is needed to run in 'cardanoMode' which
  -- is the default for cardano-cli
  defaultByronEpochSlots = 21600 :: Word64
