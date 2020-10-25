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
dt :: Double
dt = 1/365
gbm = GeometricBrownian 0.09 0.10

st :: RVar Double
st = evolve gbm (Euler dt) (0, 100) 2
sts :: RVar [Double]
sts = replicateM 1000 (sample st)


main :: IO ()
main = do
  let n = 1000000
  sts <- (V.fromList . sort) <$> replicateM n (sample st)
  let (bins, sums) = histogram 1000 sts
      dist = (V.toList $ V.zip bins sums) :: [(Double, Double)]
  toFile def "/tmp/sample.svg" $ do
    layout_title .= "100 at 1 year"
    plot (line "s(1)" [dist])
  
  
                       
  



