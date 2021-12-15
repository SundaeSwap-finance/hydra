module Hydra.Chain.Direct.ContractSpec where

import Hydra.Prelude hiding (label)
import Test.Hydra.Prelude

import qualified Cardano.Ledger.Alonzo.Data as Ledger
import qualified Cardano.Ledger.Alonzo.Scripts as Ledger
import Cardano.Ledger.Alonzo.Tools (
  evaluateTransactionExecutionUnits,
 )
import qualified Cardano.Ledger.Alonzo.TxWitness as Ledger
import qualified Cardano.Ledger.Shelley.API as Ledger
import qualified Data.Map as Map
import Data.Maybe.Strict (StrictMaybe (SNothing))
import qualified Hydra.Chain.Direct.Fixture as Fixture
import Hydra.Chain.Direct.Tx (closeTx)
import Hydra.Chain.Direct.TxSpec (mkHeadOutput)
import qualified Hydra.Contract.MockHead as MockHead
import Hydra.Ledger.Cardano (
  AlonzoEra,
  CardanoTx,
  LedgerEra,
  Tx (Tx),
  TxBody (ShelleyTxBody),
  TxBodyScriptData (TxBodyNoScriptData, TxBodyScriptData),
  Utxo,
  describeCardanoTx,
  fromLedgerTx,
  fromLedgerUtxo,
  toLedgerTx,
  toLedgerUtxo,
 )
import Plutus.V1.Ledger.Api (fromData, toData)
import Test.QuickCheck (
  Property,
  counterexample,
  forAll,
  property,
 )
import Test.QuickCheck.Instances ()
import Test.QuickCheck.Property (conjoin)

spec :: Spec
spec = describe "On-chain contracts" $ do
  describe "Close" $ do
    prop "does not survive random mutations." $
      propMutation genCloseTx

--
-- Properties
--

propMutation :: Gen (CardanoTx, Utxo) -> Property
propMutation txGenerator =
  forAll txGenerator $ \(tx, lookupUtxo) ->
    forAll arbitrary $ \mutation ->
      forAll (applyMutation tx mutation) $ \tx' ->
        conjoin
          [ propTransactionValidates (tx, lookupUtxo)
          , propTransactionDoesNotValidate (tx', lookupUtxo)
          ]

propTransactionDoesNotValidate :: (CardanoTx, Utxo) -> Property
propTransactionDoesNotValidate (tx, lookupUtxo) =
  let result =
        runIdentity $
          evaluateTransactionExecutionUnits
            Fixture.pparams
            (toLedgerTx tx)
            (toLedgerUtxo lookupUtxo)
            Fixture.epochInfo
            Fixture.systemStart
            Fixture.costModels
   in counterexample "Should have not validated" $
        case result of
          Left _ ->
            property True
          Right redeemerReport ->
            any isLeft (Map.elems redeemerReport)
              & counterexample ("Tx: " <> toString (describeCardanoTx tx))
              & counterexample ("Lookup utxo: " <> decodeUtf8 (encodePretty lookupUtxo))
              & counterexample ("Redeemer report: " <> show redeemerReport)

propTransactionValidates :: (CardanoTx, Utxo) -> Property
propTransactionValidates (tx, lookupUtxo) =
  let result =
        runIdentity $
          evaluateTransactionExecutionUnits
            Fixture.pparams
            (toLedgerTx tx)
            (toLedgerUtxo lookupUtxo)
            Fixture.epochInfo
            Fixture.systemStart
            Fixture.costModels
   in counterexample "Should have validated" $
        case result of
          Left _ ->
            property False
              & counterexample ("Tx: " <> toString (describeCardanoTx tx))
              & counterexample ("Lookup utxo: " <> decodeUtf8 (encodePretty lookupUtxo))
          Right redeemerReport ->
            all isRight (Map.elems redeemerReport)
              & counterexample ("Tx: " <> toString (describeCardanoTx tx))
              & counterexample ("Lookup utxo: " <> decodeUtf8 (encodePretty lookupUtxo))
              & counterexample ("Redeemer report: " <> show redeemerReport)

--
--
-- Mutation
--

data Mutation
  = ChangeHeadRedeemer
  deriving (Show, Generic)

instance Arbitrary Mutation where
  arbitrary = genericArbitrary

applyMutation :: CardanoTx -> Mutation -> Gen CardanoTx
applyMutation (Tx body wits) = \case
  ChangeHeadRedeemer ->
    let ShelleyTxBody era ledgerBody scripts scriptData mAuxData scriptValidity = body
        body' = ShelleyTxBody era ledgerBody scripts (alterRedeemers changeHeadRedeemer scriptData) mAuxData scriptValidity
     in pure (Tx body' wits)

changeHeadRedeemer :: (Ledger.Data era, Ledger.ExUnits) -> (Ledger.Data era, Ledger.ExUnits)
changeHeadRedeemer (dat, units) =
  case fromData (Ledger.getPlutusData dat) of
    Just (_ :: MockHead.Input) ->
      -- TODO: This needs to be arbitrary, and we need a monadic / applicative result
      (Ledger.Data (toData MockHead.Abort), units)
    Nothing ->
      (dat, units)

alterRedeemers ::
  ((Ledger.Data LedgerEra, Ledger.ExUnits) -> (Ledger.Data LedgerEra, Ledger.ExUnits)) ->
  TxBodyScriptData AlonzoEra ->
  TxBodyScriptData AlonzoEra
alterRedeemers fn = \case
  TxBodyNoScriptData -> error "TxBodyNoScriptData unexpected"
  TxBodyScriptData supportedInEra dats (Ledger.Redeemers redeemers) ->
    TxBodyScriptData supportedInEra dats (Ledger.Redeemers $ fmap fn redeemers)

--
-- Generators
--

genCloseTx :: Gen (CardanoTx, Utxo)
genCloseTx = do
  utxo <- arbitrary
  headInput <- arbitrary
  let headOutput = mkHeadOutput SNothing
      headDatum = Ledger.Data $ toData MockHead.Open
      lookupUtxo = Ledger.UTxO $ Map.singleton headInput headOutput
  pure (fromLedgerTx $ closeTx 1 utxo (headInput, headOutput, headDatum), fromLedgerUtxo lookupUtxo)