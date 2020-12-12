module Q.Options (Valuation(..), intrinsinc) where

import           Q.Types

data Valuation = Valuation {
    vPremium :: Premium
  , vDelta   :: Delta
  , vVega    :: Vega
  , vGamma   :: Gamma
} deriving (Show)


-- | intrinsinc value of an option.
intrinsinc Call f k = max (f - k) 0
intrinsinc Put  f k = max (k - f) 0
