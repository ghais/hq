module Q.Options (
    module Q.Types
  , Valuation(..)
  , Option(..)
  , EuOption(..)
  , intrinsinc
  , hasTimeValue) where

import           Q.Types
import           Numeric.IEEE                   
data Valuation = Valuation {
    vPremium :: Premium
  , vDelta   :: Delta
  , vVega    :: Vega
  , vGamma   :: Gamma
} deriving (Show)


class Option opt where
  expiry :: opt -> YearFrac   -- ^ expiry of the option
  strike :: opt -> Strike     -- ^ the strike of the option
  cp     :: opt -> OptionType -- ^ Call/Put indicator
  flipCP :: opt -> opt        -- ^ Flip a call and a put.

  cpi    :: (Num a) => opt -> a
  cpi opt = if cp opt == Call then 1 else -1 
  
data EuOption = EuOption YearFrac Strike OptionType
instance Option EuOption where
  expiry (EuOption t _ _) = t
  strike (EuOption _ k _) = k
  cp     (EuOption _ _ q) = q
  flipCP (EuOption t k q) = (EuOption t k (succ q))

-- | intrinsinc value of an option.
intrinsinc :: (Option opt) => Forward -> DF -> opt -> Premium
intrinsinc (Forward f) df opt = df `discount` (Premium $ max (q*(f - k)) 0) where
  (Strike k) = strike opt
  q          = cpi opt


-- | returns True if the undiscounted option premium is greater
--   than the undiscounted 'intrinsinc' (F - K)
hasTimeValue f df opt p = (intrinsinc f df opt) < (df `undiscount` p)

