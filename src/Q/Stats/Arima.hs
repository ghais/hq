{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGuAGE MultiParamTypeClasses #-}
{-# LANGuAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell, GADTs, RankNTypes #-}

module Q.Stats.Arima where
import Data.Foldable
import Numeric.LinearAlgebra
import Control.Monad.State
import Data.Functor.Identity
import Data.RVar
import Data.Random
import System.Random.Mersenne.Pure64
import Data.Random.Source
import Q.Stats.TimeSeries
import Data.Time
--import Control.Monad.Trnas.Identity
import Data.RVar
import Data.Random.Distribution
import Statistics.Sample
import Data.Random.Distribution.T
import Data.Random.Distribution.Poisson

data Ewma d = Ewma Double d

--ll :: (Ewma d) -> [DataPoint Double] -> (Double -> Double)
ll (Ewma lambda d) datapoints = mapM ll_ datapoints where
  ll_ :: DataPoint LocalTime Double -> State Double Double
  ll_ x@(DataPoint _ v) = do
    vart <- get
    let vart2 = lambda * vart + (1 - lambda) * v * v
    put vart2
    return $ logPdf d (sqrt (v  * v / vart))


--forecast :: (Distribution d Double) => (Ewma d) -> Int ->
forecast :: forall d. (Distribution d Double) => Ewma (d Double) -> StateT Double RVar Double
forecast (Ewma lambda d) = do
  y <- lift $ rvar d
  vart <- get
  let vart2 = lambda * vart + (1 - lambda) * y * y
  put vart2
  return (y * sqrt vart)


--forecastN :: Distribution d Double => Ewma (d Double) -> Int -> Double -> RVar ([Double], Double)
forecastN ewma var0 n =  sample $ runStateT (replicateM n (forecast ewma)) var0

