{-# LANGUAGE MultiParamTypeClasses #-}
module Q.Options.ImpliedVol.StrikeInterpolation where

import           Data.Coerce
import qualified Numeric.GSL.Interpolation as GSL
import           Q.Interpolation
import           Q.SortedVector
import           Q.Types

data StrikeInterpolation = Linear
                         | CubicNatural
                         | CubicAkima
                         | CubicMonotone

data StrikeExtrapolation = Constant
                         | ConstantGradient
                         | ConstantCurvature

instance InterpolatorV StrikeInterpolation Strike Vol where
  interpolateV Linear        (SortedVector strikes) vols (Strike k) =
    Vol $ GSL.evaluateV GSL.Linear (coerce strikes) (coerce vols) k

  interpolateV CubicNatural  (SortedVector strikes) vols (Strike k) =
    Vol $ GSL.evaluateV GSL.CSpline  (coerce strikes) (coerce vols) k

  interpolateV CubicAkima    (SortedVector strikes) vols (Strike k) =
    Vol $ GSL.evaluateV GSL.Akima  (coerce strikes) (coerce vols) k

  interpolateV CubicMonotone (SortedVector strikes) vols (Strike k) =
    Vol $ GSL.evaluateV GSL.Steffen (coerce strikes) (coerce vols) k

