{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module Q.ImpliedVol.LetsBeRational where

import Numeric.IEEE (epsilon, minNormal, maxFinite)
import Statistics.Distribution (cumulative, density, quantile)
import Statistics.Distribution.Normal (standard)
import Data.Number.Erf
import Foreign.C.Types
import Q.Types
import Q.Options
import Data.Coerce (coerce)
import Q.BlackScholes
foreign import ccall "lets_be_rational.h implied_volatility_from_a_transformed_rational_guess" c_lbr :: CDouble -> CDouble  -> CDouble -> CDouble  -> CDouble  -> CDouble

euImpliedVol :: OptionType -> Forward -> Strike -> YearFrac -> Rate -> Premium -> Vol
euImpliedVol cp (Forward f) (Strike k) (YearFrac t) (Rate r) (Premium p) =
  coerce $ c_lbr (CDouble p) (CDouble f) (CDouble k) (CDouble t) (CDouble (cpi cp))


f = Spot 100
cp = Call
t = YearFrac 1
r = Rate 0.0
vol = Vol 0.1
bs = BlackScholes f r vol
k = atmf bs t
p = vPremium $ eucall bs k t
vol' = euImpliedVol Call (Forward 100) k t r p
