{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}

module Q.BlackScholes (
    BlackScholes(..)
  , atmf
  , euOption
  , eucall
  , euput
  , module Q.Options
) where
import           Control.Monad.State
import           Data.Random                    hiding (Gamma)
import           Data.Time
import           Numeric.RootFinding
import           Q.ContingentClaim.Options
import           Q.MonteCarlo
import           Q.Options
import           Q.Stochastic.Discretize
import           Q.Stochastic.Process
import           Q.Time
import           Q.Types
import           Statistics.Distribution        (cumulative, density)
import           Statistics.Distribution.Normal (standard)

dcf = dcYearFraction ThirtyUSA




-- | Parameters for a simplified black scholes equation.
data BlackScholes = BlackScholes {
    bsSpot :: Spot -- ^ The asset's spot on the valuation date.
  , bsRate :: Rate   -- ^ Risk free rate.
  , bsVol  :: Vol    -- ^ Volatility.
} deriving Show



instance Model BlackScholes Double where
  discountFactor BlackScholes{..} t1 t2 = return $ exp (scale dt bsRate)
    where dt = t2 - t1

  evolve (BlackScholes spot (Rate r) (Vol sigma)) (YearFrac t) = do
    (YearFrac t0, s0) <- get
    let dt = t - t0
    dw <- (lift stdNormal)::StateT (YearFrac, Double) RVar Double
    let st = s0 * exp ((r - 0.5 * sigma * sigma) * dt + sigma * dw * sqrt dt)
    put (YearFrac t, st)
    return st

atmf :: BlackScholes -> YearFrac -> Strike
atmf BlackScholes{..} t = Strike $ s / d where
  (Rate d) = exp (scale t (-bsRate))
  (Spot s) = bsSpot



-- | European option valuation with black scholes.
euOption ::  BlackScholes -> OptionType -> Strike -> YearFrac -> Valuation
euOption bs@BlackScholes{..} cp k t = Valuation premium delta vega gamma where
  (Strike f)  = atmf bs t
  n           = cumulative standard
  (Vol sigmaSqt) = scale t bsVol
  (Rate df)   = exp (scale t (-bsRate))
  (Spot s)    = bsSpot
  d1          = (dPlus  f bsRate bsVol k t)
  d2          = (dMinus f bsRate bsVol k t)
  nd1         = n d1
  nd2         = n d2
  callDelta   = nd1
  putDelta    = - (n (-d1))
  vega        = Vega $ (density standard d1 ) * s * sigmaSqt
  gamma       = Gamma $ (density standard d1) / (s * sigmaSqt)
  premium  = Premium $ case cp of
    Call -> df * (f * nd1 - nd2 * k')
    Put  -> df * (n (-d2) * k' - n (-d1) * f)
    where (Strike k') = k
  delta | cp == Call = Delta $ callDelta
        | cp == Put  = Delta $ putDelta

-- | see 'euOption'
euput bs =  euOption bs Put

-- | see 'euOption'
eucall bs = euOption bs Call

dPlus f (Rate r) (Vol sigma) (Strike k) (YearFrac t)  = recip (sigma * sqrt t) * (log (f/k) + (0.5 * sigma * sigma) * t)
dMinus f (Rate r) (Vol sigma) (Strike k) (YearFrac t) = recip (sigma * sqrt t) * (log (f/k) - (0.5 * sigma * sigma) * t)


corradoMillerIniitalGuess bs@BlackScholes{..} cp (Strike k) (YearFrac t) (Premium premium) =
  (recip $ sqrt t) * ((sqrt (2 * pi)/ (s + discountedStrike)) + (premium - (s - discountedStrike)/2) + sqrt ((premium - (s - discountedStrike)/2)**2 - ((s - discountedStrike)**2/pi))) where
    discountedStrike = k * (exp $ (-r) * t)
    (Rate r) = bsRate
    (Spot s) = bsSpot


