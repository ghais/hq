{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}
module Q.Stochastic.Process        
        where
import Data.List (foldl')
import Data.RVar
import Data.Random
import Control.Monad

class (Num c) => Linear c where
  scale :: Double -> c -> c

instance Linear Double where
  scale  = (*)

  

type Time = Double
-- | Discretization of stochastic process over given interval
class Discretize d where
  dDrift  :: (StochasticProcess a b) => a -> d -> (Time, b) -> RVar b
  dDiff   :: (StochasticProcess a b) => a -> d -> (Time, b) -> RVar b
  dDt     :: (StochasticProcess a b) => a -> d -> (Time, b) -> Time


-- | 1D Stochastic process
class Linear b => StochasticProcess a b where
  drift  :: a -> (Time, b) -> RVar b
  diff   :: a -> (Time, b) -> RVar b

  evolve :: (Discretize d) => a -> d -> (Time, b) -> Time -> RVar b
  evolve p disc s0@(t0, x0) t = do
    if t0 >= t then return x0 else do
      s'@(t', b') <- evolve' p disc s0
      if t' >= t then return b' else evolve p disc s' t
      
    
  -- TODO: evolve should take a timeline.
  evolve' :: (Discretize d) => a -> d -> (Time, b) -> RVar (Time, b)
  evolve' process discr s@(t, b) = do
          let !newT = t + dDt process discr s
          !newX <- (b + ) <$> liftM2 (+) (dDrift process discr s) (dDiff process discr s)
          return (newT, newX)
   

-- | Path as list of Dots
type Path b = [(Time, b)]


-- | Geometric Brownian motion
data GeometricBrownian = GeometricBrownian {
        gbDrift :: Double,
        gbDiff  :: Double
        } deriving (Show)

instance StochasticProcess GeometricBrownian Double where
  drift p (_, x) = return $ gbDrift p * x -- drift is prpotional to the spot.
  diff  p (_, x) = return $ gbDiff p  * x -- diffisuion is also prportional to the spot.

  evolve GeometricBrownian{..}  _  b@(_, s0) t = do
    w <- stdNormal
    return $ s0 * exp ((gbDrift - 0.5 * gbDiff * gbDiff)* t + gbDiff * w * sqrt t)

  
-- | Ito process
data ItoProcess = ItoProcess {
        ipDrift :: (Time, Double) -> Double,
        ipDiff  :: (Time, Double) -> Double
}

instance StochasticProcess ItoProcess Double where
  drift :: ItoProcess -> (Time, Double) -> RVar Double
  drift  process = return . (ipDrift process)

  diff :: ItoProcess -> (Time, Double) -> RVar Double
  diff process = return . (ipDrift process)


-- | Square-root process
data SquareRootProcess = SquareRootProcess {
        srpSpeed :: Double,
        srpMean  :: Double,
        srpSigma :: Double
        } deriving (Show)

instance StochasticProcess SquareRootProcess Double where
       drift p (_, x) = return $ srpSpeed p * (srpMean p - x)
       diff  p (_, x) = return $ srpSigma p * sqrt x

-- | Ornstein-Uhlenbeck process
data OrnsteinUhlenbeckProcess = OrnsteinUhlenbeckProcess {
        oupSpeed :: Double,
        oupLevel :: Double,
        oupSigma :: Double
        } deriving (Show)

instance StochasticProcess OrnsteinUhlenbeckProcess Double where
        drift p (_, x) = return $ oupSpeed p * (oupLevel p - x)
        diff  p _         = return $ oupSigma p

