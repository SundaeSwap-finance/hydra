{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-deferred-type-errors #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}

module Hydra.Logic where

import Cardano.Prelude

import Control.Monad.Class.MonadTime (DiffTime)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Hydra.Ledger (
  Amount,
  Committed,
  Ledger (applyTransaction, canApply, getUTxO),
  LedgerState,
  ParticipationToken (..),
  Party,
  UTxO,
  ValidationError,
  ValidationResult (Invalid, Valid),
  initLedgerState,
 )

data Event tx
  = ClientEvent (ClientRequest tx)
  | NetworkEvent (NetworkEvent tx)
  | OnChainEvent OnChainTx
  | ShouldPostFanout
  deriving (Eq, Show)

data NetworkEvent tx
  = MessageReceived (HydraMessage tx)
  | NetworkConnected
  deriving (Eq, Show)

data Effect tx
  = ClientEffect (ClientResponse tx)
  | NetworkEffect (HydraMessage tx)
  | OnChainEffect OnChainTx
  | Delay DiffTime (Event tx)

deriving instance Eq tx => Eq (UTxO tx) => Eq (Effect tx)
deriving instance Show tx => Show (UTxO tx) => Show (Effect tx)

data ClientRequest tx
  = Init [Party]
  | Commit Amount
  | NewTx tx
  | Close
  | Contest
  deriving (Eq, Read, Show)

data ClientResponse tx
  = NodeConnectedToNetwork
  | ReadyToCommit
  | HeadIsOpen (UTxO tx)
  | HeadIsClosed (UTxO tx)
  | HeadIsFinalized (UTxO tx)
  | CommandFailed
  | ReqTxReceived tx
  | TxInvalid tx

deriving instance Eq tx => Eq (UTxO tx) => Eq (ClientResponse tx)
deriving instance Show tx => Show (UTxO tx) => Show (ClientResponse tx)

data HydraMessage tx
  = ReqTx tx
  | AckTx
  | ConfTx
  | ReqSn
  | AckSn
  | ConfSn
  deriving (Eq, Show)

data OnChainTx
  = InitTx (Set.Set ParticipationToken)
  | CommitTx ParticipationToken Natural
  | CollectComTx
  | CloseTx
  | ContestTx
  | FanoutTx
  deriving (Eq, Show, Read)

data HeadState tx = HeadState
  { headParameters :: HeadParameters
  , headStatus :: HeadStatus tx
  }

deriving instance Eq (UTxO tx) => Eq (SimpleHeadState tx) => Eq (HeadState tx)
deriving instance Show (UTxO tx) => Show (SimpleHeadState tx) => Show (HeadState tx)

data HeadStatus tx
  = InitState
  | CollectingState PendingCommits Committed
  | OpenState (SimpleHeadState tx)
  | ClosedState (UTxO tx)
  | FinalState

deriving instance Eq (UTxO tx) => Eq (SimpleHeadState tx) => Eq (HeadStatus tx)
deriving instance Show (UTxO tx) => Show (SimpleHeadState tx) => Show (HeadStatus tx)

data SimpleHeadState tx = SimpleHeadState
  { confirmedLedger :: LedgerState tx
  }

deriving instance Eq (UTxO tx) => Eq (LedgerState tx) => Eq (SimpleHeadState tx)
deriving instance Show (UTxO tx) => Show (LedgerState tx) => Show (SimpleHeadState tx)

type PendingCommits = Set ParticipationToken

-- | Contains at least the contestation period and other things.
newtype HeadParameters = HeadParameters {contestationPeriod :: DiffTime}
  deriving (Eq, Show)

-- | Decides when, how often and who is in charge of creating snapshots.
data SnapshotStrategy = SnapshotStrategy

-- | Assume: We know the party members and their verification keys. These need
-- to be exchanged somehow, eventually.
createHeadState :: [Party] -> HeadParameters -> SnapshotStrategy -> HeadState tx
createHeadState _ parameters _ = HeadState parameters InitState

-- | Preliminary type for collecting errors occurring during 'update'. Might
-- make sense to merge this (back) into 'Outcome'.
data LogicError tx
  = InvalidEvent (Event tx) (HeadState tx)
  | InvalidState (HeadState tx)
  | LedgerError ValidationError

deriving instance (Eq (HeadState tx), Eq (Event tx)) => Eq (LogicError tx)
deriving instance (Show (HeadState tx), Show (Event tx)) => Show (LogicError tx)

data Outcome tx
  = NewState (HeadState tx) [Effect tx]
  | Error (LogicError tx)
  | Wait

newState :: HeadParameters -> HeadStatus tx -> [Effect tx] -> Outcome tx
newState p s = NewState (HeadState p s)

data Environment = Environment
  { -- | This is the p_i from the paper
    party :: Party
  }

-- | The heart of the Hydra head logic, a handler of all kinds of 'Event' in the
-- Hydra head. This may also be split into multiple handlers, i.e. one for hydra
-- network events, one for client events and one for main chain events, or by
-- sub-'State'.
update ::
  Show (LedgerState tx) =>
  Show (UTxO tx) =>
  Show tx =>
  Environment ->
  Ledger tx ->
  HeadState tx ->
  Event tx ->
  Outcome tx
update Environment{party} ledger (HeadState p st) ev = case (st, ev) of
  (InitState, ClientEvent (Init parties)) ->
    newState p InitState [OnChainEffect (InitTx $ makeAllTokens parties)]
  (InitState, OnChainEvent (InitTx tokens)) ->
    newState p (CollectingState tokens mempty) [ClientEffect ReadyToCommit]
  --
  (CollectingState remainingTokens _, ClientEvent (Commit amount)) ->
    case findToken remainingTokens party of
      Nothing ->
        panic $ "you're not allowed to commit (anymore): remainingTokens : " <> show remainingTokens <> ", partiyIndex:  " <> show party
      Just pt -> newState p st [OnChainEffect (CommitTx pt amount)]
  (CollectingState remainingTokens committed, OnChainEvent (CommitTx pt amount)) ->
    let remainingTokens' = Set.delete pt remainingTokens
        newCommitted = Map.insert pt amount committed
        newHeadState = CollectingState remainingTokens' newCommitted
     in if canCollectCom party pt remainingTokens'
          then newState p newHeadState [OnChainEffect CollectComTx]
          else newState p newHeadState []
  (CollectingState{}, OnChainEvent CollectComTx) ->
    let ls = initLedgerState ledger
     in newState
          p
          (OpenState $ SimpleHeadState ls)
          [ClientEffect $ HeadIsOpen $ getUTxO ledger ls]
  --
  (OpenState _, OnChainEvent CommitTx{}) ->
    Error (InvalidEvent ev (HeadState p st)) -- HACK(SN): is a general case later
  (OpenState{}, ClientEvent Close) ->
    newState p st [OnChainEffect CloseTx, Delay (contestationPeriod p) ShouldPostFanout]
  (OpenState SimpleHeadState{confirmedLedger}, ClientEvent (NewTx tx)) ->
    case canApply ledger confirmedLedger tx of
      Invalid _ -> newState p st [ClientEffect $ TxInvalid tx]
      Valid -> newState p st [NetworkEffect $ ReqTx tx]
  (OpenState headState, NetworkEvent (MessageReceived (ReqTx tx))) ->
    case applyTransaction ledger (confirmedLedger headState) tx of
      Right newLedgerState ->
        newState
          p
          (OpenState $ headState{confirmedLedger = newLedgerState})
          [ClientEffect $ ReqTxReceived tx]
      Left{} -> panic "TODO: how is this case handled?"
  (OpenState SimpleHeadState{confirmedLedger}, OnChainEvent CloseTx) ->
    let utxo = getUTxO ledger confirmedLedger
     in newState p (ClosedState utxo) [ClientEffect $ HeadIsClosed utxo]
  (ClosedState{}, ShouldPostFanout) ->
    newState p st [OnChainEffect FanoutTx]
  (ClosedState utxos, OnChainEvent FanoutTx) ->
    newState p FinalState [ClientEffect $ HeadIsFinalized utxos]
  --
  (_, NetworkEvent NetworkConnected) ->
    newState p st [ClientEffect NodeConnectedToNetwork]
  (_, ClientEvent{}) ->
    newState p st [ClientEffect CommandFailed]
  _ -> panic $ "UNHANDLED EVENT: on " <> show party <> " of event " <> show ev <> " in state " <> show st

canCollectCom :: Party -> ParticipationToken -> Set ParticipationToken -> Bool
canCollectCom party pt remainingTokens = null remainingTokens && thisToken pt == party

makeAllTokens :: [Party] -> Set ParticipationToken
makeAllTokens parties = Set.fromList $ map (ParticipationToken total) parties
 where
  total = fromIntegral $ length parties

findToken :: Set ParticipationToken -> Party -> Maybe ParticipationToken
findToken allTokens party =
  find (\t -> thisToken t == party) allTokens
