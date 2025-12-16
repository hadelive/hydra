module Hydra.Tx.HeadParameters where

import Hydra.Prelude

import Data.List (nub)
import Hydra.Plutus.Orphans ()
import Hydra.Tx.ContestationPeriod (ContestationPeriod)
import Hydra.Tx.Party (Party (..))
import PlutusLedgerApi.V3 (CurrencySymbol)

-- | Contains the head's parameters as established in the initial transaction.
data HeadParameters = HeadParameters
  { contestationPeriod :: ContestationPeriod
  , parties :: [Party] -- NOTE(SN): The order of this list is important for leader selection.
  , ponderaPolicyId :: Maybe CurrencySymbol -- ^ Optional Pondera NFT policy ID for snapshot verification
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance Arbitrary HeadParameters where
  arbitrary = dedupParties <$> genericArbitrary
   where
    dedupParties HeadParameters{contestationPeriod, parties, ponderaPolicyId} =
      HeadParameters{contestationPeriod, parties = nub parties, ponderaPolicyId}
