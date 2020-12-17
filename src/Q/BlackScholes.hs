{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE DisambiguateRecordFields #-}
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
import qualified Q.Black76 as B76

dcf = dcYearFraction ThirtyUSA

-- | Parameters for a simplified black scholes equation.
data BlackScholes = BlackScholes {
    bsSpot :: Spot Double -- ^ The asset's spot on the valuation date.
  , bsRate :: Rate Double -- ^ Risk free rate.
  , bsVol  :: Vol  Double -- ^ Volatility.
} deriving Show



instance Model BlackScholes Double where
  discountFactor BlackScholes{..} t1 t2 = return $ DF (exp (-r * dt))
    where (YearFrac dt) = t2 - t1
          (Rate r) = bsRate

  evolve (BlackScholes spot (Rate r) (Vol sigma)) (YearFrac t) = do
    (YearFrac t0, s0) <- get
    let dt = t - t0
    dw <- (lift stdNormal)::StateT (YearFrac, Double) RVar Double
    let st = s0 * exp ((r - 0.5 * sigma * sigma) * dt + sigma * dw * sqrt dt)
    put (YearFrac t, st)
    return st

atmf :: BlackScholes -> YearFrac -> Strike Double
atmf BlackScholes{..} t = Strike f where
  df          = Q.Types.discountFactor t bsRate
  (Forward f) = forward df bsSpot


-- | European option valuation with black scholes.
euOption :: BlackScholes -> OptionType -> Strike Double -> YearFrac -> Valuation Double
euOption bs@BlackScholes{..} cp k t = B76.euOption (B76.Black76 f df bsVol t) cp k t where
  df             = Q.Types.discountFactor t bsRate
  f              = forward df bsSpot

-- | see 'euOption'
euput bs =  euOption bs Put

-- | see 'euOption'
eucall bs = euOption bs Call




corradoMillerIniitalGuess bs@BlackScholes{..} cp (Strike k) (YearFrac t) (Premium premium) =
  (recip $ sqrt t) * ((sqrt (2 * pi)/ (s + discountedStrike)) + (premium - (s - discountedStrike)/2) + sqrt ((premium - (s - discountedStrike)/2)**2 - ((s - discountedStrike)**2/pi))) where
    discountedStrike = k * (exp $ (-r) * t)
    (Rate r) = bsRate
    (Spot s) = bsSpot


