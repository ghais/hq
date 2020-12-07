module Q.ImpliedVol.LetsBeRational where

import Numeric.IEEE (epsilon, minDenormal, maxFinite)
import Statistics.Distribution (cumulative, density, quantile)
import Statistics.Distribution.Normal (standard)
import Data.Number.Erf


twoPi                    = 6.283185307179586476925286766559005768394338798750::Double
sqrtPiOver2              = 1.253314137315500251207882642405522626503493370305::Double
sqrtThree                = 1.732050807568877293527446341505872366942805253810::Double
sqrtOneOverThree         = 0.577350269189625764509148780501957455647601751207::Double
twoPiOverSqrtTwentySeven = 1.209199576156145233729385505094770488189377498728::Double
piOverSix                = 0.523598775598298873077107230546583814032861566563::Double
oneOverSqrtTwo           = 0.707106781186547524400844362104849039284835937688::Double
oneOverSqrtTwoPi         = 0.398942280401432677939946059934381868475858631164::Double
sqrtTwoPi                = 2.506628274631000502415765284811045253006986740610::Double

sqrtEpsilon              = sqrt epsilon::Double
fourthRootEpsilon        = sqrt sqrtEpsilon
eightsRootEpsilon        = sqrt fourthRootEpsilon
sixteenthRootEpsilon     = sqrt eightsRootEpsilon

sqrtDblMin               = sqrt minDenormal::Double
sqrtDblMax               = sqrt maxFinite::Double

denormalizationCutoff               = 0
ivMaxIterations                     = 2
asymptoticExpansionAccurayThreshold = -10


isBelowHorizon x = abs x < denormalizationCutoff

houseHolderFactor newton halley hh3 = (1 + 0.5 * halley * newton ) / (1.0 + newton * (halley + hh3 * newton / 6))

normalisedIntrinsic x θ | θ * x <= 0 = 0
                        | x2 < 0.98*fourthRootEpsilon =
                          abs $ max (qSign*x*(1+x2*((1.0/24.0)+x2*((1.0/1920.0)+x2*((1.0/322560.0)+(1.0/92897280.0)*x2))))) 0
                        | otherwise = abs $ max (qSign * (bMax - oneOverBMax)) 0
  where x2          = x * x
        qSign       = signum θ
        bMax        = exp $ 0.5 * x
        oneOverBMax = recip bMax

normalisedIntrinsicCall x = normalisedIntrinsic x 1

normCdf = cumulative standard
normPdf = density standard
invnorm  = quantile standard


asymptoticExpansionOfNormalisedBlackCall h t = let
  e = (t / h) * (t / h)
  r = ((h + t) * (h - t))
  q = (h / r) * (h / r)
  b = oneOverSqrtTwoPi  * exp ((-0.5 * (h*h + t*t))) * (t / r) * (asymptoticExpansionSum e q)
  in abs $ max b 0


smallTExpansionOfNormalisedBlackCall h t =
  let a  = 1 + h * (0.5 * sqrtTwoPi) * erfcx ((-oneOverSqrtTwo) * h)
      w  = t * t
      b  = oneOverSqrtTwoPi * exp ((-0.5 * (h*h + t*t))) * expansion t a w h
  in abs $ max b 0



smallTExpansionOfNormalisedBlackThreshold = 2 * sixteenthRootEpsilon
normCdfAsymptoticExpansionFirstThreshold = -10.0
normCdfAsymptoticExpansionSecondThreshold = (-1) / sqrtEpsilon





normalisedBlackcCallUsingErfcx h t =
  let b  = 0.5 * exp (-0.5*(h*h+t*t)) * (erfcx (-oneOverSqrtTwo*(h+t)) - erfcx(-oneOverSqrtTwo*(h-t)))
  in abs $ max b 0

normalisedBlackCallUsingNormCdf x s =
  let h    = x / s
      t    = 0.5 * s
      bMax = exp $ 0.5 * x
      b    = normCdf(h+t)*bMax - normCdf(h-t)/bMax
  in abs $ max b 0.0



normalisedBlackCall x s | x > 0  = normalisedIntrinsicCall x + normalisedBlackCall (-x) s
                        | s <= ax * denormalizationCutoff = normalisedIntrinsicCall x
                        | x < s * asymptoticExpansionAccurayThreshold &&
                          0.5*s*s+x < s * (smallTExpansionOfNormalisedBlackThreshold +asymptoticExpansionAccurayThreshold) =
                            asymptoticExpansionOfNormalisedBlackCall (x/s) (0.5 * s)
                        | 0.5 * s < smallTExpansionOfNormalisedBlackThreshold =
                            smallTExpansionOfNormalisedBlackCall (x/s) (0.5*s)
                        | x+0.5*s*s > s * 0.85 =
                            normalisedBlackCallUsingNormCdf x s
                        | otherwise =
                            normalisedBlackcCallUsingErfcx (x/s) (0.5 * s)
  where ax = abs x

normaliedVega x s | ax <= 0 = oneOverSqrtTwoPi * exp ((-0.125)*s*s)
                   | s <= 0 || s <= ax * sqrtDblMin = 0
                   | otherwise                   = oneOverSqrtTwoPi * (exp $ (-0.5) *  (x/s)**2 + (0.5*s)**2)
  where ax = abs x

normalisedBlack x s q | q < 0     = normalisedBlackCall (-x) s
                      | otherwise = normalisedBlackCall x s


black :: Double -> Double -> Double -> Double -> Double -> Double
black f k sigma t q | q  * (f - k) > 0 = intrinsic + black f k sigma t (-q)
                    | otherwise        = max  intrinsic  ((sqrt f * sqrt k) * (normalisedBlack (log(f/k))  (sigma * sqrt t) q))
  where intrinsic | q < 0     = abs $ max (k - f) 0
                  | otherwise = abs $ max (f - k) 0

minimumRationalCubicControlParameterValue :: Double
minimumRationalCubicControlParameterValue = -(1 - (sqrt epsilon))
maximumRationalCubicControlParameterValue = 2 / (epsilon * epsilon)





isZero x = abs x < minDenormal

rationalCubicInterpolation :: Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double
rationalCubicInterpolation x x_l x_r y_l y_r d_l d_r r 
  | h <= 0 =
      0.5 * (y_l + y_r)
  | not (r >= maximumRationalCubicControlParameterValue) =
      let t = (x - x_l) / h
          omt = 1 - t
          t2 = t * t
          omt2 = omt * omt
      in (y_r*t2*t + (r*y_r-h*d_r)*t2*omt + (r*y_l+h*d_l)*t*omt2 + y_l*omt2*omt) / (1 + (r-3)*t*omt)
  | otherwise =
      let t = (x - x_l) / h
      in y_r * t + y_l *(1 - t)
  where h = x_r - x_l
      


rationalCubicControlParameterToFitSecondDerivativeAtLeftSide x_l x_r y_l y_r d_l d_r second_derivative_l
  | isZero numerator   = 0
  | isZero denominator = if numerator > 0 then 
                           maximumRationalCubicControlParameterValue
                         else
                           minimumRationalCubicControlParameterValue
  | otherwise          =  numerator / denominator
  where h = x_r - x_l
        numerator   = 0.5*h*second_derivative_l + (d_r - d_l)
        denominator = (y_r-y_l)/h - d_l
        
  
rationalCubicControlParameterToFitSecondDerivativeAtRightSide x_l x_r y_l y_r d_l d_r second_derivative_r
  | isZero numerator   = 0
  | isZero denominator = if numerator > 0 then 
                           maximumRationalCubicControlParameterValue
                         else
                           minimumRationalCubicControlParameterValue
  | otherwise          =  numerator / denominator
  where h = x_r - x_l
        numerator   = 0.5*h*second_derivative_r + (d_r - d_l)
        denominator = d_r - (y_r-y_l)/h


minimumRationalCubicControlParameter d_l d_r s preferShapePreservationOverSmoothness
  | (not monotonic) && (not convex) && (not concave) =
      minimumRationalCubicControlParameterValue
  | otherwise = max minimumRationalCubicControlParameterValue (max r1 r2)
  where monotonic = d_l*s >= 0 && d_r*s >= 0
        convex    = d_l <= s && s <= d_r
        concave   = d_l >= s && s >= d_r
        d_r_m_d_l = d_r - d_l
        d_r_m_s   = d_r - s
        s_m_d_l   = s - d_l        
        r1 | isZero s                              = (d_r + d_l) / s
           | preferShapePreservationOverSmoothness = maximumRationalCubicControlParameterValue
           | otherwise                             = maxFinite
        r2 | convex || concave = if (isZero s_m_d_l) || (isZero d_r_m_s) then
                                  max (abs (d_r_m_d_l/d_r_m_s)) (abs(d_r_m_d_l/s_m_d_l))
                                else if preferShapePreservationOverSmoothness then
                                       maximumRationalCubicControlParameterValue
                                     else r1
           | monotonic && preferShapePreservationOverSmoothness = maximumRationalCubicControlParameterValue
           | otherwise                                         = r1
          
rationalCubicControlParameterToFitSecondDerivativeAtLeftSside x_l x_r y_l y_r d_l d_r second_derivative_l
  | isZero numerator   = 0
  | isZero denominator = if numerator > 0 then
                           maximumRationalCubicControlParameterValue
                         else
                           minimumRationalCubicControlParameterValue
  | otherwise          = denominator / numerator
  where h           = x_r - x_l
        numerator   = 0.5*h*second_derivative_l + (d_r - d_l)
        denominator = (y_r-y_l)/h - d_l


convexRationalCubicControlParameterToFitSecondDerivativeAtLeftSide x_l x_r y_l y_r d_l d_r second_derivative_l preferShapePreservationOverSmoothness =
  let r     = rationalCubicControlParameterToFitSecondDerivativeAtLeftSide x_l x_r y_l y_r d_l d_r second_derivative_l
      r_min = minimumRationalCubicControlParameter d_l d_r ((y_r-y_l)/(x_r-x_l)) preferShapePreservationOverSmoothness
  in max r r_min

convexRationalCubicControlParameterToFitSecondDerivativeAtRightSide x_l x_r y_l y_r d_l d_r second_derivative_r preferShapePreservationOverSmoothness =
  let r = rationalCubicControlParameterToFitSecondDerivativeAtRightSide x_l x_r y_l y_r d_l d_r second_derivative_r
      r_min = minimumRationalCubicControlParameter d_l d_r ((y_r-y_l)/(x_r-x_l)) preferShapePreservationOverSmoothness
  in max r r_min


computeFLowerMapAndFirstTwoDerivatives x s =
  let ax         = abs x
      z          = sqrtOneOverThree * ax / s
      y          = z * z
      s2         = s * s
      capitalPhi = normCdf (-z)
      lowerPhi   = normCdf z      
      fpp        = piOverSix * y / (s2 * s) * capitalPhi * (8*sqrtThree*s*ax + (3*s2*(s2-8)-8*x*x)*capitalPhi/lowerPhi) * exp (2*y+0.25*s2)
      phi2       = lowerPhi * lowerPhi
      fp         = twoPi * y * phi2 * exp (y+0.125*s*s)
  in if isBelowHorizon s then
       (0, 1, fpp)
     else
       if isBelowHorizon x then
         (0, fp, fpp)
       else
         (twoPiOverSqrtTwentySeven  * ax * (phi2 * capitalPhi), fp, fpp)

inverseFLowerMap x f =
  if isBelowHorizon f then
    0
  else abs (x / (sqrtThree * invnorm((f/(twoPiOverSqrtTwentySeven * (abs x)))** (1/3))))


computeFUpperMapAndFirstTwoDerivatives x s
  | isBelowHorizon x = (f, -(0.5), 0)
  | otherwise        = (f, (-0.5) * exp (0.5*w), sqrtTwoPi * (exp (w+0.125*s*s)) * w / s)
  where f = normCdf ((-0.5) * s)
        w = (x/s)**2









  
asymptoticExpansionSum e q = (2.0 + q*(-6.0E0-2.0*e+3.0*q*(1.0E1+e*(2.0E1+2.0*e)+5.0*q*(-1.4E1+e*(-7.0E1+e*(-4.2E1-2.0*e))+7.0*q*(1.8E1+e*(1.68E2+e*(2.52E2+e*(7.2E1+2.0*e)))+9.0*q*(-2.2E1+e*(-3.3E2+e*(-9.24E2+e*(-6.6E2+e*(-1.1E2-2.0*e))))+1.1E1*q*(2.6E1+e*(5.72E2+e*(2.574E3+e*(3.432E3+e*(1.43E3+e*(1.56E2+2.0*e)))))+1.3E1*q*(-3.0E1+e*(-9.1E2+e*(-6.006E3+e*(-1.287E4+e*(-1.001E4+e*(-2.73E3+e*(-2.1E2-2.0*e))))))+1.5E1*q*(3.4E1+e*(1.36E3+e*(1.2376E4+e*(3.8896E4+e*(4.862E4+e*(2.4752E4+e*(4.76E3+e*(2.72E2+2.0*e)))))))+1.7E1*q*(-3.8E1+e*(-1.938E3+e*(-2.3256E4+e*(-1.00776E5+e*(-1.84756E5+e*(-1.51164E5+e*(-5.4264E4+e*(-7.752E3+e*(-3.42E2-2.0*e))))))))+1.9E1*q*(4.2E1+e*(2.66E3+e*(4.0698E4+e*(2.3256E5+e*(5.8786E5+e*(7.05432E5+e*(4.0698E5+e*(1.08528E5+e*(1.197E4+e*(4.2E2+2.0*e)))))))))+2.1E1*q*(-4.6E1+e*(-3.542E3+e*(-6.7298E4+e*(-4.90314E5+e*(-1.63438E6+e*(-2.704156E6+e*(-2.288132E6+e*(-9.80628E5+e*(-2.01894E5+e*(-1.771E4+e*(-5.06E2-2.0*e))))))))))+2.3E1*q*(5.0E1+e*(4.6E3+e*(1.0626E5+e*(9.614E5+e*(4.08595E6+e*(8.9148E6+e*(1.04006E7+e*(6.53752E6+e*(2.16315E6+e*(3.542E5+e*(2.53E4+e*(6.0E2+2.0*e)))))))))))+2.5E1*q*(-5.4E1+e*(-5.85E3+e*(-1.6146E5+e*(-1.77606E6+e*(-9.37365E6+e*(-2.607579E7+e*(-4.01166E7+e*(-3.476772E7+e*(-1.687257E7+e*(-4.44015E6+e*(-5.9202E5+e*(-3.51E4+e*(-7.02E2-2.0*e))))))))))))+2.7E1*q*(5.8E1+e*(7.308E3+e*(2.3751E5+e*(3.12156E6+e*(2.003001E7+e*(6.919458E7+e*(1.3572783E8+e*(1.5511752E8+e*(1.0379187E8+e*(4.006002E7+e*(8.58429E6+e*(9.5004E5+e*(4.7502E4+e*(8.12E2+2.0*e)))))))))))))+2.9E1*q*(-6.2E1+e*(-8.99E3+e*(-3.39822E5+e*(-5.25915E6+e*(-4.032015E7+e*(-1.6934463E8+e*(-4.1250615E8+e*(-6.0108039E8+e*(-5.3036505E8+e*(-2.8224105E8+e*(-8.870433E7+e*(-1.577745E7+e*(-1.472562E6+e*(-6.293E4+e*(-9.3E2-2.0*e))))))))))))))+3.1E1*q*(6.6E1+e*(1.0912E4+e*(4.74672E5+e*(8.544096E6+e*(7.71342E7+e*(3.8707344E8+e*(1.14633288E9+e*(2.07431664E9+e*(2.33360622E9+e*(1.6376184E9+e*(7.0963464E8+e*(1.8512208E8+e*(2.7768312E7+e*(2.215136E6+e*(8.184E4+e*(1.056E3+2.0*e)))))))))))))))+3.3E1*(-7.0E1+e*(-1.309E4+e*(-6.49264E5+e*(-1.344904E7+e*(-1.4121492E8+e*(-8.344518E8+e*(-2.9526756E9+e*(-6.49588632E9+e*(-9.0751353E9+e*(-8.1198579E9+e*(-4.6399188E9+e*(-1.6689036E9+e*(-3.67158792E8+e*(-4.707164E7+e*(-3.24632E6+e*(-1.0472E5+e*(-1.19E3-2.0*e)))))))))))))))))*q)))))))))))))))))

expansion t a w h = 2 * t * (a + w*((-1+3*a+a*h2)/6+w*((-7+15*a+h2*(-1+10*a+a*h2))/120+w*((-57+105*a+h2*(-18+105*a+h2*(-1+21*a+a*h2)))/5040+w*((-561+945*a+h2*(-285+1260*a+h2*(-33+378*a+h2*(-1+36*a+a*h2))))/362880+w*((-6555+10395*a+h2*(-4680+17325*a+h2*(-840+6930*a+h2*(-52+990*a+h2*(-1+55*a+a*h2)))))/39916800+((-89055+135135*a+h2*(-82845+270270*a+h2*(-20370+135135*a+h2*(-1926+25740*a+h2*(-75+2145*a+h2*(-1+78*a+a*h2))))))*w)/6227020800.0)))))) where h2 = h * h
