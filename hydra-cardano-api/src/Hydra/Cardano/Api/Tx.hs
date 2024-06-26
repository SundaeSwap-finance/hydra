module Hydra.Cardano.Api.Tx where

import Hydra.Cardano.Api.Prelude

import Cardano.Api.UTxO qualified as UTxO
import Cardano.Ledger.Allegra.Scripts (translateTimelock)
import Cardano.Ledger.Alonzo qualified as Ledger
import Cardano.Ledger.Alonzo.Scripts qualified as Ledger
import Cardano.Ledger.Alonzo.TxAuxData (translateAlonzoTxAuxData)
import Cardano.Ledger.Alonzo.TxWits qualified as Ledger
import Cardano.Ledger.Api (
  AlonzoPlutusPurpose (..),
  AsIx (..),
  ConwayPlutusPurpose (..),
  EraTx (mkBasicTx),
  addrTxOutL,
  addrTxWitsL,
  auxDataHashTxBodyL,
  auxDataTxL,
  bodyTxL,
  bootAddrTxWitsL,
  collateralInputsTxBodyL,
  collateralReturnTxBodyL,
  dataTxOutL,
  datsTxWitsL,
  feeTxBodyL,
  inputsTxBodyL,
  isValidTxL,
  mintTxBodyL,
  mkBasicTxBody,
  mkBasicTxOut,
  mkBasicTxWits,
  networkIdTxBodyL,
  outputsTxBodyL,
  rdmrsTxWitsL,
  referenceInputsTxBodyL,
  referenceScriptTxOutL,
  reqSignerHashesTxBodyL,
  scriptIntegrityHashTxBodyL,
  scriptTxWitsL,
  totalCollateralTxBodyL,
  valueTxOutL,
  vldtTxBodyL,
  withdrawalsTxBodyL,
  witsTxL,
 )
import Cardano.Ledger.Api qualified as Ledger
import Cardano.Ledger.Babbage qualified as Ledger
import Cardano.Ledger.Babbage.TxWits (upgradeTxDats)
import Cardano.Ledger.BaseTypes (maybeToStrictMaybe)
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Conway.Scripts (PlutusScript (..))
import Cardano.Ledger.Conway.Scripts qualified as Conway
import Cardano.Ledger.Conway.TxBody qualified as Ledger
import Cardano.Ledger.Plutus.Data (upgradeData)
import Control.Lens ((&), (.~), (^.))
import Data.Bifunctor (bimap)
import Data.Functor ((<&>))
import Data.Map qualified as Map
import Data.Maybe (mapMaybe)
import Data.Set qualified as Set
import Hydra.Cardano.Api.TxIn (mkTxIn, toLedgerTxIn)

-- * Extras

-- | Explicit downgrade from Conway to Babbage era.
--
-- NOTE: This is not a complete mapping and does silently drop things like
-- protocol updates, certificates and voting procedures.
convertConwayTx :: Tx ConwayEra -> Tx BabbageEra
convertConwayTx =
  fromLedgerTx . convert . toLedgerTx
 where
  convert :: Ledger.Tx (Ledger.ConwayEra StandardCrypto) -> Ledger.Tx (Ledger.BabbageEra StandardCrypto)
  convert tx =
    mkBasicTx (translateBody $ tx ^. bodyTxL)
      & witsTxL .~ translateWits (tx ^. witsTxL)
      & isValidTxL .~ tx ^. isValidTxL
      & auxDataTxL .~ (translateAlonzoTxAuxData <$> tx ^. auxDataTxL)

  translateBody ::
    Ledger.ConwayTxBody (Ledger.ConwayEra StandardCrypto) ->
    Ledger.BabbageTxBody (Ledger.BabbageEra StandardCrypto)
  translateBody body =
    mkBasicTxBody
      & inputsTxBodyL .~ body ^. inputsTxBodyL
      & outputsTxBodyL .~ (translateTxOut <$> body ^. outputsTxBodyL)
      & feeTxBodyL .~ body ^. feeTxBodyL
      & withdrawalsTxBodyL .~ body ^. withdrawalsTxBodyL
      & auxDataHashTxBodyL .~ body ^. auxDataHashTxBodyL
      -- NOTE: not considering 'updateTxBodyL' as upstream also does not upgrade it
      -- NOTE: not considering 'certsTxBodyL' as we are not interested in it
      & vldtTxBodyL .~ body ^. vldtTxBodyL
      & mintTxBodyL .~ body ^. mintTxBodyL
      & collateralInputsTxBodyL .~ body ^. collateralInputsTxBodyL
      & reqSignerHashesTxBodyL .~ body ^. reqSignerHashesTxBodyL
      & scriptIntegrityHashTxBodyL .~ body ^. scriptIntegrityHashTxBodyL
      & networkIdTxBodyL .~ body ^. networkIdTxBodyL
      & referenceInputsTxBodyL .~ body ^. referenceInputsTxBodyL
      & totalCollateralTxBodyL .~ body ^. totalCollateralTxBodyL
      & collateralReturnTxBodyL .~ (translateTxOut <$> body ^. collateralReturnTxBodyL)

  translateTxOut ::
    Ledger.BabbageTxOut (Ledger.ConwayEra StandardCrypto) ->
    Ledger.BabbageTxOut (Ledger.BabbageEra StandardCrypto)
  translateTxOut out =
    mkBasicTxOut (out ^. addrTxOutL) (out ^. valueTxOutL)
      & dataTxOutL .~ (upgradeData <$> out ^. dataTxOutL)
      & referenceScriptTxOutL .~ (out ^. referenceScriptTxOutL >>= maybeToStrictMaybe . translateScript)

  translateWits ::
    Ledger.AlonzoTxWits (Ledger.ConwayEra StandardCrypto) ->
    Ledger.AlonzoTxWits (Ledger.BabbageEra StandardCrypto)
  translateWits wits =
    mkBasicTxWits
      & addrTxWitsL .~ wits ^. addrTxWitsL
      & bootAddrTxWitsL .~ wits ^. bootAddrTxWitsL
      & scriptTxWitsL .~ Map.mapMaybe translateScript (wits ^. scriptTxWitsL)
      & datsTxWitsL .~ upgradeTxDats (wits ^. datsTxWitsL)
      & rdmrsTxWitsL .~ translateRdmrs (wits ^. rdmrsTxWitsL)

  translateScript ::
    Ledger.AlonzoScript (Ledger.ConwayEra StandardCrypto) ->
    Maybe (Ledger.AlonzoScript (Ledger.BabbageEra StandardCrypto))
  translateScript = \case
    Ledger.TimelockScript ts -> Just . Ledger.TimelockScript $ translateTimelock ts
    Ledger.PlutusScript ps -> case ps of
      ConwayPlutusV1 p1 -> Just . Ledger.PlutusScript $ BabbagePlutusV1 p1
      ConwayPlutusV2 p2 -> Just . Ledger.PlutusScript $ BabbagePlutusV2 p2
      ConwayPlutusV3{} -> Nothing

  translateRdmrs ::
    Ledger.Redeemers (Ledger.ConwayEra StandardCrypto) ->
    Ledger.Redeemers (Ledger.BabbageEra StandardCrypto)
  translateRdmrs (Ledger.Redeemers redeemerMap) =
    Ledger.Redeemers
      . Map.fromList
      $ mapMaybe
        ( \(purpose, (dat, units)) -> do
            p' <- translatePlutusPurpose purpose
            pure (p', (upgradeData dat, units))
        )
      $ Map.toList redeemerMap

  translatePlutusPurpose ::
    Conway.ConwayPlutusPurpose Ledger.AsIx (Ledger.ConwayEra StandardCrypto) ->
    Maybe (Ledger.AlonzoPlutusPurpose Ledger.AsIx (Ledger.BabbageEra StandardCrypto))
  translatePlutusPurpose = \case
    ConwaySpending (AsIx ix) -> Just $ AlonzoSpending (AsIx ix)
    ConwayMinting (AsIx ix) -> Just $ AlonzoMinting (AsIx ix)
    ConwayCertifying (AsIx ix) -> Just $ AlonzoCertifying (AsIx ix)
    ConwayRewarding (AsIx ix) -> Just $ AlonzoRewarding (AsIx ix)
    ConwayVoting{} -> Nothing
    ConwayProposing{} -> Nothing

-- | Sign transaction using the provided secret key
-- It only works for tx not containing scripts.
-- You can't sign a script utxo with this.
signTx ::
  IsShelleyBasedEra era =>
  SigningKey PaymentKey ->
  Tx era ->
  Tx era
signTx signingKey (Tx body wits) =
  makeSignedTransaction (witness : wits) body
 where
  witness = makeShelleyKeyWitness shelleyBasedEra body (WitnessPaymentKey signingKey)

-- | Create a transaction spending all given `UTxO`.
txSpendingUTxO :: UTxO -> Tx Era
txSpendingUTxO utxo =
  fromLedgerTx $
    mkBasicTx
      ( mkBasicTxBody
          & inputsTxBodyL .~ (toLedgerTxIn `Set.map` inputs)
      )
 where
  inputs = UTxO.inputSet utxo

-- | Get the UTxO that are produced by some transaction.
-- XXX: Defined here to avoid cyclic module dependency
utxoProducedByTx :: Tx Era -> UTxO
utxoProducedByTx tx =
  UTxO.fromPairs $
    zip [0 ..] (txOuts body)
      <&> bimap (mkTxIn tx) toCtxUTxOTxOut
 where
  TxBody body = getTxBody tx

-- | Get explicit fees allocated to a transaction.
txFee' :: Tx era -> Coin
txFee' (getTxBody -> TxBody body) =
  case txFee body of
    TxFeeExplicit _ y -> y

-- * Type Conversions

-- | Convert a cardano-api 'Tx' into a matching cardano-ledger 'Tx'.
toLedgerTx ::
  Tx era ->
  Ledger.Tx (ShelleyLedgerEra era)
toLedgerTx (ShelleyTx _era tx) = tx

-- | Convert a cardano-ledger's 'Tx' in the Babbage era into a cardano-api 'Tx'.
fromLedgerTx ::
  IsShelleyBasedEra era =>
  Ledger.Tx (ShelleyLedgerEra era) ->
  Tx era
fromLedgerTx =
  ShelleyTx shelleyBasedEra
