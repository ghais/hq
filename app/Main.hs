{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveTraversable #-}
{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where
import Prelude hiding (filter)
import Q.ContingentClaim.Options
import Q.BlackScholes
import Q.Stochastic.Process
import Q.Stochastic.Discretize
import Data.Random
import Data.RVar
import Control.Monad
import Data.List hiding (filter)
import Statistics.Sample.Histogram
import Q.MonteCarlo
import Data.Time
import Data.Foldable
import Control.Monad.State
import qualified Numeric.LinearAlgebra as V
import Graphics.Vega.VegaLite hiding (sample, repeat)
import qualified Data.Text as T
import Q.Stats.TimeSeries
import Q.Time.DayCounter
import Q.Time
import Data.Time
import Q.Types
import qualified Q.Util.File as QF
import qualified Graphics.Gnuplot.Advanced as GP

import qualified Graphics.Gnuplot.MultiPlot as MultiPlot

import qualified Graphics.Gnuplot.Frame as Frame
import qualified Graphics.Gnuplot.Frame.OptionSet as Opts
import qualified Graphics.Gnuplot.Frame.OptionSet.Style as OptsStyle
import qualified Graphics.Gnuplot.Frame.OptionSet.Histogram as Histogram

import qualified Graphics.Gnuplot.Graph as Graph

import qualified Graphics.Gnuplot.Plot.ThreeDimensional as Plot3D
import qualified Graphics.Gnuplot.Graph.ThreeDimensional as Graph3D

import qualified Graphics.Gnuplot.Plot.TwoDimensional as Plot2D
import qualified Graphics.Gnuplot.Graph.TwoDimensional as Graph2D
import Graphics.Gnuplot.Plot.TwoDimensional (linearScale, )

import qualified Graphics.Gnuplot.LineSpecification as LineSpec

import qualified Data.Time as Time


dt :: Double
dt = 1/365
gbm = GeometricBrownian 0.0 0.15



plotPayouts :: IO ()
plotPayouts = do
  let strike      = 100.0
      lowStrike   = 95.0
      highStrike  = 105.0
      expiry      = fromGregorian 2015 3 31
      spots       = [90, 90.01..110]
      calls       = map (vanillaPayout Call strike) spots
      puts        = map (vanillaPayout Put strike) spots
      callSpreads = map (spreadPayout Call lowStrike highStrike) spots
      putSpreads  = map (spreadPayout Put lowStrike highStrike) spots
      straddles   = map (straddlePayout strike) spots

      payouts = dataFromColumns []
        . dataColumn "Spot" (Numbers spots)
        . dataColumn "Call" (Numbers calls)
        . dataColumn "Put"  (Numbers puts)
        . dataColumn "Call Spread"  (Numbers callSpreads)
        . dataColumn "Put Spread"  (Numbers putSpreads)
        . dataColumn "Straddle" (Numbers straddles)

      enc = encoding
        . position X [PName "Spot", PmType Quantitative]
      encVanillaCall = enc
        . position Y [PName "Call", PmType Quantitative]
      encVanillaPut  = enc
        . position Y [PName "Put", PmType Quantitative]
      encCallSpread = enc
        . position Y [PName "Call Spread", PmType Quantitative]
      encPutSpread = enc
        . position Y [PName "Put Spread", PmType Quantitative]
      encStraddle = enc
        . position Y [PName "Straddle", PmType Quantitative]        
                     
      vl = toVegaLite [payouts [], vlConcat [ asSpec [encVanillaCall [], mark Line []]
                                            , asSpec [encVanillaPut  [], mark Line []]
                                            , asSpec [encCallSpread  [], mark Line []]
                                            , asSpec [encPutSpread   [], mark Line []]
                                            , asSpec [encStraddle    [], mark Line []]
                                            ]
                      ]
  toHtmlFile "/tmp/payouts.html" vl

-- Option delta is the sensitivity of the option to the underlying:
-- \(dV/dS\).
--
-- for a fixed time and vol what is the delta per relative strike.
-- How does delta look as a function of time
-- How does delta look as a function of vol?



-- How does delta look like as a function of the spot?
-- Plot call delta against log relative strike.
-- Plot put  delta against log relative strike.
studyDelta :: IO ()
studyDelta = do
  let today           = fromGregorian 2015 3 31
      expiry          = YearFrac 1
      vol             = 0.6
      spot            =  100
      strikes         = map Strike [80, 81..120]
      vols            = [0.01,0.06..1]
      expiries        = map YearFrac [0.1,0.2..2]
      relativeStrikes = [log (k/(atmf bs expiry)) | k <- strikes]
      r               = 0.01
      bs              = BlackScholes spot r vol
      delta cp k      = vDelta (euOption bs cp k expiry)
      pv    cp k      = vPremium (euOption bs cp k expiry)
      row (vol,k,t)     = [show today,
                         show t,
                         show $log (k / atmf bs t),
                         show r,
                         show vol,
                         show $ pv Call k,
                         show $ pv Put k,
                         show $ delta Call k,
                         show $ delta Put k
                        ]
      header           = ["VD","Expiry","RelativeStrike", "IR", "Vol", "cPV", "pPV", "cDelta", "pDelta"]
                        
  QF.write (map row [(vol, k, t) | vol <- vols, k <- strikes, t <- expiries]) header "/tmp/data/delta.csv"
  
  let mydata = dataFromUrl "delta.csv" [Parse [ ( "Expiry", FoDate "%Y-%m-%d" ), ( "VD", FoDate "%Y-%m-%d" ) ]] 

  -- !This is \(dV/dS\) for different strikes, same expiry, interest rate,  
  let l1 = toVegaLite [mydata , layers, width 800, height 600, res, trans] where
      calls = encoding
        . position X [PName "RelativeStrike"    , PmType Quantitative]
        . position Y [PName "cDelta" , PmType Quantitative, PTitle "Calls", PAxis [AxTitleColor "green"]]
      puts  = encoding
        . position X [PName "RelativeStrike"    , PmType Quantitative]
        . position Y [PName "pDelta"  , PmType Quantitative, PTitle "Puts", PAxis [AxTitleColor "red"]]
      layers = layer [ asSpec [calls [], mark Line [MColor "green"]]
                     , asSpec [puts  [], mark Line [MColor "red"]]
                     ]
      trans = transform . filter (FExpr "datum.Expiry == datetime(2015,3,15)") $ []
      res = resolve . resolution (RScale [(ChY, Independent)]) $ []
                        
  toHtmlFile "/tmp/data/deltas.html" l1
      

-- | Plot call option premiums, deltas, payoffs across a differnt
-- range of strikes.
plotCallOptions :: IO ()
plotCallOptions = do
  let today   = fromGregorian 2015 3 31
      expiry  = YearFrac 1.0
      spot    = 100
      r       = 0
      vol     = 0.01
      bs      = BlackScholes spot r vol
      k       = atmf bs expiry
      cp      = Call
  let strikes = [95, 95.01..105]
      values  = [] -- map (vPremium . euOption cp bs expiry) strikes
      deltas  = [] --map (vDelta   . euOption cp bs expiry) strikes
      pvs = dataFromColumns []
        . dataColumn "Strikes" (Numbers strikes)
        . dataColumn "Premiums"     (Numbers values)
        . dataColumn "Deltas"  (Numbers deltas)
  let encPvs = encoding
        . position X [PName "Strikes", PmType Quantitative]
        . position Y [PName "Premiums"    , PmType Quantitative]
      encDeltas = encoding
        . position X [PName "Strikes", PmType Quantitative]
        . position Y [PName "Deltas", PmType Quantitative]
      layers = layer [ asSpec [ encPvs     [], mark Line [MColor "blue"]]
                     , asSpec [ encDeltas  [], mark Line [MColor "red"]]
                     ]
      res = resolve . resolution (RScale [(ChY, Independent)])
      vl     = toVegaLite [pvs [], layers, width 800, res [], height 400]
  toHtmlFile "/tmp/callOptions.html" vl



testPlot = do
  let dvals = dataSequenceAs 0 6.28 0.1 "x"
      trans = transform
              . calculateAs "sin(datum.x)" "sinX"
              . calculateAs "cos(datum.x)" "cosX"
      enc = encoding
            . position X [PName "x", PmType Quantitative]
      encCos = enc . position Y [PName "cosX", PmType Quantitative]
      encSin = enc . position Y [PName "sinX", PmType Quantitative]

      vl = toVegaLite [ dvals
                      , trans []
                      , vlConcat [ asSpec [encCos [], mark Line []]
                                 , asSpec [encSin [], mark Line []]
                                 ]
                      ]
  toHtmlFile "/tmp/test.html" vl           
                    
plotTrajectories :: IO ()
plotTrajectories = do
  let dt = 1/365
      today = fromGregorian 2015 3 31
      dates = [d | d <- fmap (flip addDays today) [0, 365]]
      dts = tail $ map (\d -> dcYearFraction ThirtyUSA today d) dates
      n = 64000
      ids = mconcat $ fmap (replicate $ length dates) [T.pack $ show id | id <- [1..n]]
  trajectories <- sample $ trajectories n (Euler dt) gbm  (100::Double) dts (repeat stdNormal)
  let trajectories' = dataFromColumns [ Parse [ ( "Date", FoDate "%Y%m%d" ) ] ]
                      . dataColumn "Date" (Strings  (mconcat (replicate n (map dayToString dates))))
                      . dataColumn "St" (Numbers $ mconcat trajectories)
                      . dataColumn "ID" (Strings ids)
      calls = mark Line 
      enc = encoding
            . position X [ PName "Date", PmType Temporal]
            . position Y [ PName "St", PmType Quantitative, PScale [SDomain (DNumbers [50, 150])] ]
            . color [ MName "ID", MmType Nominal, MLegend []]
      vl = toVegaLite [trajectories' [], enc [], mark Line [], width 1000, height 600]
  toHtmlFile "/tmp/a.html" vl



simple2d :: Plot2D.T Double Double
simple2d =
   Plot2D.function Graph2D.lines
      (linearScale 100 (-10,10)) sin
main  = GP.plotDefault simple2d
  
                      
