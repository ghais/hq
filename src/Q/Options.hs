module Q.Options (Valuation(..), intrinsinc) where

import           Q.Types

data Valuation = Valuation {
    vPremium :: Premium
  , vDelta   :: Delta
  , vVega    :: Vega
  , vGamma   :: Gamma
} deriving (Show)


-- | intrinsinc value of an option.
intrinsinc :: OptionType -> Forward -> Strike -> Premium
intrinsinc Call (Forward f) (Strike k) = Premium $ max (f - k) 0
intrinsinc Put  (Forward f) (Strike k) = Premium $ max (k - f) 0
