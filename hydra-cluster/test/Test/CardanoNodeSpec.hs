module Test.CardanoNodeSpec where

import Hydra.Prelude
import Test.Hydra.Prelude

import CardanoNode (
  RunningNode (..),
  getCardanoNodeVersion,
  withCardanoNodeDevnet,
 )

import CardanoClient (queryTipSlotNo, CardanoClient(..))
import Hydra.Cardano.Api (NetworkId (Testnet), NetworkMagic (NetworkMagic), unFile)
import Hydra.Logging (showLogsOnFailure)
import System.Directory (doesFileExist)

spec :: Spec
spec = do
  -- NOTE: We also hard-code the cardano-node version here to allow prevent
  -- false positives test errors in case someone uses an "untested" /
  -- different than in shell.nix version of cardano-node and cardano-cli.
  it "has expected cardano-node version available" $
    getCardanoNodeVersion >>= (`shouldContain` "8.1.2")

  -- NOTE: We hard-code the expected networkId here to detect any change to the
  -- genesis-shelley.json
  it "withCardanoNodeDevnet does start a block-producing devnet within 5 seconds" $
    failAfter 5 $
      showLogsOnFailure $ \tr -> do
        withTempDir "hydra-cluster" $ \tmp -> do
          withCardanoNodeDevnet tr tmp $ \cardanoClient -> do
            doesFileExist (unFile $ nodeSocketClientOnline cardanoClient) `shouldReturn` True
            networkIdClientOnline cardanoClient `shouldBe` Testnet (NetworkMagic 42)
            -- Should produce blocks (tip advances)
            slot1 <- queryTipSlotNo cardanoClient
            threadDelay 1
            slot2 <- queryTipSlotNo cardanoClient
            slot2 `shouldSatisfy` (> slot1)
