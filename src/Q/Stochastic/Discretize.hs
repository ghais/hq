{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}

module Q.Stochastic.Discretize
        where

import Q.Stochastic.Process
import Data.RVar
import Data.Functor
-- | Euler discretization of stochastic processes
data Euler = Euler { eDt :: Double }
        deriving (Show, Eq)

-- | Euler end-point discretization of stochastic processes
data EndEuler = EndEuler { eeDt :: Double }
        deriving (Show, Eq)

instance  Discretize Euler where
  dDrift p Euler{..} s0 = drift p s0 <&> (scale eDt)

  dDt _ Euler{..} _  = eDt

  dDiff  p Euler{..} b = (diff p b) <&> (scale (sqrt eDt))

instance (forall a b. StochasticProcess a b) => Discretize EndEuler where
  dDrift p EndEuler{..} s0@(t0, x0) = drift p (t0 + eeDt, x0) <&> (scale eeDt)
  dDiff  p EndEuler{..}  s0@(t0, x0) =  diff  p (t0 + eeDt, x0) <&> (scale (sqrt eeDt))
  dDt    _ e _   = eeDt e
