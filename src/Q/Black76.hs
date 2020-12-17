{-# LANGUAGE RecordWildCards #-}
module Q.Black76
  (
    Black76(..)
  , euOption
  , dPlus
  , dMinus
  ) where


import Q.Options
import           Statistics.Distribution        (cumulative, density)
import           Statistics.Distribution.Normal (standard)

data Black76 = Black76 {
    b76F   :: Forward Double
  , b76DF  :: DF Double
  , b76Vol :: Vol Double
  , b76T   :: YearFrac
} deriving (Show, Read)

dPlus (Forward f) (Vol sigma) (Strike k) (YearFrac t)  = recip (sigma * sqrt t) * (log (f/k) + (0.5 * sigma * sigma) * t)
dMinus (Forward f) (Vol sigma) (Strike k) (YearFrac t) = recip (sigma * sqrt t) * (log (f/k) - (0.5 * sigma * sigma) * t)


-- | European option valuation with black scholes.
euOption :: Black76 -> OptionType -> (Strike Double) -> YearFrac -> Valuation Double
euOption Black76 {..} cp k t = Valuation premium delta vega gamma where
  (DF df)        = b76DF
  (Forward f)    = b76F
  n              = cumulative standard
  (Vol sigmaSqt) = scale t b76Vol
  d1             = (dPlus  b76F b76Vol k t)
  d2             = (dMinus b76F b76Vol k t)
  nd1            = n d1
  nd2            = n d2
  callDelta      = df * nd1
  putDelta       = df * (-(n (-d1)))
  vega           = Vega    $ df * (density standard d1 ) * f * sigmaSqt
  gamma          = Gamma   $ df * (density standard d1) / (f * sigmaSqt)
  premium        = Premium $ case cp of
    Call -> df * (f * nd1 - nd2 * k')
    Put  -> df * (n (-d2) * k' - n (-d1) * f)
    where (Strike k') = k
  delta          | cp == Call = Delta $ callDelta
                 | cp == Put  = Delta $ putDelta
