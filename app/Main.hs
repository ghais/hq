{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}

module Main where

import Q.Stochastic.Process
import Q.Stochastic.Discretize
import Data.Random
import Data.RVar
import Control.Monad
import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Diagrams
import Data.List
import qualified Data.Vector as V
import Statistics.Sample.Histogram
import Q.MonteCarlo
dt :: Double
dt = 1/365
gbm = GeometricBrownian 0.05 0.1



dts = [dt, 2 * dt..1]
p :: RVar [[Double]]
p = replicateM 20 $ generatePath (Euler dt) gbm  100 dts (repeat stdNormal)


r' x y z = sqrt $ x^2 + y^2 + z^2
efield sign x y = ( sign*x/r,sign*y/r) where r = r' x y 10
bfield sign x y = (-sign*y/r^2,sign*x/r^2) where r = r' x y 10
square a s = [(x,y) | x <- range, y <- range] where range = [-a,-a+s..a] :: [Double]
add (x1,y1) (x2,y2) = (x1+x2,y1+y2)

ef (x,y) = efield 1 (x-20) y `add` efield (-1) (x+20) y
bf (x,y) = bfield 1 (x-20) y `add` bfield (-1) (x+20) y
grid = square 30 3

main :: IO ()
main = do
  paths <- (map (zip dts)) <$> sample p
  toFile def "/tmp/trajectory.svg" $ do
    layout_title .= "100 at 1 year"
    plot (line "S(t)" paths)


  
                       



