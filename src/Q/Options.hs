module Q.Options (Valuation(..), intrinsinc, hasTimeValue) where

import           Q.Types
import           Numeric.IEEE                   
data Valuation = Valuation {
    vPremium :: Premium
  , vDelta   :: Delta
  , vVega    :: Vega
  , vGamma   :: Gamma
} deriving (Show)


-- | intrinsinc value of an option.
intrinsinc :: OptionType -> Forward -> Strike -> DF -> Premium
intrinsinc Call (Forward f) (Strike k) = (`discount` (Premium $ max (f - k) 0))
intrinsinc Put  (Forward f) (Strike k) = (`discount` (Premium $ max (k - f) 0))

-- | returns True if the undiscounted option premium is greater
--   than the undiscounted 'intrinsinc' (F - K)
hasTimeValue :: 
     OptionType
  -> Forward
  -> Strike
  -> Premium
  -> DF
  -> Bool
hasTimeValue cp f k p df = (intrinsinc cp f k df) < (df `undiscount` p)
