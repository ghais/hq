{-|
Module      : StudyBlackScholes
Description : A visual study of black scholes pricing function.
-}

{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
module StudyDelta where

import qualified Data.ByteString.Lazy   as B (writeFile)
import           Data.Csv
import qualified Data.Text              as T
import           Data.Time
import           GHC.Generics           (Generic)
import           Graphics.Vega.VegaLite
import           Prelude                hiding (filter)
import qualified Q.BlackScholes         as BS
import           Q.Plotting
import           Q.Types
-- | Data holder to map to a file.
data OptionValuation = OptionValuation {
     strike            :: Strike     -- ^ Option Strike.
   , tte               :: YearFrac   -- ^ Expiry
   , callPut           :: OptionType -- ^ Call/Put
   , ir                :: Rate       -- ^ Risk free rate.
   , vol               :: Vol        -- ^ Implied vol.
   , premium           :: Premium    -- ^ Options premium.
   , delta             :: Delta      -- ^ Option's delta.
   , vega              :: Vega       -- ^ Option's vega.
   , gamma             :: Gamma      -- ^ Option's gamma.
   , logRelativeStrike :: Strike -- log relative Strike
} deriving (Generic, Show)

-- we want to save records to a csv file.
instance ToNamedRecord   OptionValuation
instance FromNamedRecord OptionValuation
instance DefaultOrdered  OptionValuation

-- | Price a black scholes option.
valuation :: BS.BlackScholes -> OptionType -> Strike -> YearFrac -> OptionValuation
valuation bs@BS.BlackScholes{..} cp k t =
  OptionValuation k t cp bsRate bsVol premium delta vega gamma (log (k / f)) where
  f       = BS.atmf bs t
  premium = BS.vPremium v
  delta   = BS.vDelta   v
  vega    = BS.vVega    v
  gamma   = BS.vGamma   v
  v       = BS.euOption bs cp k t

-- | combinator for different option parameters.
generate :: OptionType -> [Spot] -> [Strike] -> [YearFrac] -> [Rate] -> [Vol] -> [OptionValuation]
generate cp spots strikes expiries rates vols = [
        valuation bs cp k t |
            bs   <- [BS.BlackScholes spot r vol | r <- rates, vol <- vols, spot <- spots]
          , k    <- strikes
          , t    <- expiries
          ]

-- | see 'generate'
generateCallsAndPuts spot strikes expiries rates vols =
  (generate Call spot strikes expiries rates vols)
  <>
  (generate Put spot strikes expiries rates vols)

-- | create some sample data.
createCsv :: FilePath -> IO ()
createCsv f = do
  let spots       = [Spot 100]
      strikes    = [Strike k | k <- [1,2..300]]
      expiries   = [YearFrac 0.1, YearFrac 1, YearFrac 2, YearFrac 10, YearFrac 20]
      rates      = [Rate 0.01]
      vols       = [Vol 0.1]
      valuations = generateCallsAndPuts spots strikes expiries rates vols
  B.writeFile f (encodeDefaultOrderedByName valuations)

-- | plot premium/vega/gamma/ as function of delta for an option where
-- spot = 100, r = 0.01, vol = 0.1 and expiry in [0.1, 1, 2, 10, 20] years.
createVegaLite :: String -> FilePath -> IO ()
createVegaLite dataPath outPath = do
  createCsv dataPath
  let mydata = dataFromUrl (T.pack dataPath) []
  let years = zip [0.1, 1, 10, 20] colorPairs
  -- section for delta
      perTenor x y (t, (callColor, putColor)) = asSpec [
          encoding
           . position X [PName x, PmType Quantitative]
           . position Y [PName y, PmType Quantitative]
           . color [
              MName "callPut",
              MmType Nominal,
              MScale  [SRange  (RStrings [callColor, putColor])],
              MLegend [LTitle (T.pack ((show t) ++ " years"))]] $ []
        , transform . (filter (FEqual "tte" $ Number t)) $ []
        , mark Line []
        , title (T.concat [x, " (VS) ", y]) []
        ]
      res = resolve
          . resolution (RScale [(ChColor, Independent)])
          $ []
      deltas = [
        asSpec [layer $ map (perTenor "delta" "premium") years, res, width 900, height 600],
        asSpec [layer $ map (perTenor  "delta" "delta") years, res, width 900, height 600],
        asSpec [layer $ map (perTenor  "delta" "vega") years, res, width 900, height 600],
        asSpec [layer $ map (perTenor  "delta""gamma") years,  res, width 900, height 600]
        ]
      logrelativeStrike = [
        asSpec [layer $ map (perTenor "logRelativeStrike" "premium") years, res, width 900, height 600],
        asSpec [layer $ map (perTenor  "logRelativeStrike" "delta") years, res, width 900, height 600],
        asSpec [layer $ map (perTenor  "logRelativeStrike" "vega") years, res, width 900, height 600],
        asSpec [layer $ map (perTenor  "logRelativeStrike" "gamma") years,  res, width 900, height 600]
        ]
      
      vl = toVegaLite [
          mydata
        , hConcat [asSpec [vConcat deltas], asSpec [vConcat logrelativeStrike]]
        , res

        ]

  toHtmlFile outPath vl


