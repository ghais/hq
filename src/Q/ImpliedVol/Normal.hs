module Q.ImpliedVol.Normal where
import           Q.Bachelier
import           Q.Types
import           Statistics.Distribution        (cumulative, density)
import           Statistics.Distribution.Normal (standard)
import Numeric.IEEE (epsilon, minNormal, maxFinite)
import           Numeric.RootFinding
import Data.Default.Class

-- | Method to use to calculate the normal implied vol.
data Method = Jackel     -- ^ Jackel analytical formula approximation.
            | ChoKimKwak -- ^ J. Choi, K kim, and M. Kwak (2009)
            | RootFinding

-- | Default method implementation of 'euImpliedVolWith' using Jackel.
euImpliedVol = euImpliedVolWith Jackel

-- | Calcualte the bachelier option implied vol of a european option.
euImpliedVolWith :: Method -> OptionType -> Forward -> Strike -> YearFrac -> Rate -> Premium -> Vol

euImpliedVolWith Jackel cp (Forward f) (Strike k) (YearFrac t) (Rate r) (Premium p)
  -- Case of no time value. We return 0 vol.
  | p == intrinsinc cp f k = Vol $ 0
  -- Case of ATM. Solve directly. (TODO: configurable threshold?)
  | abs (k - f) <= 1e-10 = Vol $ p * sqrt2Pi / (sqrt t)
  -- Case of ITM option. Calcualte vol of the out of the money option with Put-Call-Parity.
  | phiStarTilde >= 0
    = euImpliedVol (succ cp) (Forward f) (Strike k) (YearFrac t) (Rate r) (Premium p')
  -- General case for an out of the money option.
  | otherwise  = let
      ẋ      = if phiStarTilde < c then
                 let g = 1 / (phiStarTilde - 0.5)
                     ξ = (0.032114372355 - (g**2)*(0.016969777977 - (g**2)*(2.6207332461E-3-(9.6066952861E-5)*g**2)))
                         /
                         (1-(g**2)*(0.6635646938 - (g**2)*(0.14528712196 - 0.010472855461*g**2)))
                 in g * (1 / sqrt2Pi + ξ*g**2)
               else
                 let h = sqrt $ (-log (-phiStarTilde))
                 in (9.4883409779-h*(9.6320903635-h*(0.58556997323 + 2.1464093351*h)))
                    /
                    (1-h*(0.65174820867 + h*(1.5120247828 + 6.6437847132E-5*h)))
      c       = (-0.001882039271)
      x       = ẋ + (3*q * ẋ * ẋ * (2 - q * ẋ * (2 + ẋ*ẋ)))
                    /
                    (6 + q*ẋ * ((-12) + ẋ *(6*q + ẋ * ((-6)*q*ẋ*(3+ẋ*ẋ)))))
      phiXBarTilde = (cumulative standard ẋ) + (density standard ẋ)/ẋ
      q       =  (phiXBarTilde-phiStarTilde)/ (density standard ẋ)
    in Vol $ (abs (k - f)) / (abs (x * sqrt t))
  where phiStarTilde = negate $ (abs (p - (max (theta * (f - k)) 0))) / (abs (k - f))
        theta        = if cp == Call then 1 else -1
        phiTilde     = (-theta) * p / (k - f)
        p'           = cpi * df * (f - k) + p
        cpi          = fromIntegral $ fromEnum cp --call put indicartor.
        df           = exp $ (-r) * t
        sqrt2Pi      = 2.506628274631000


euImpliedVolWith ChoKimKwak cp (Forward f) (Strike k) (YearFrac t) (Rate r) (Premium p) =
  let df              = exp $ (-r) * t
      forwardPremium  = p / df
      straddlePremium = case cp of Call -> 2 * forwardPremium - (f - k)
                                   Put  -> 2 * forwardPremium + (f - k)
      nu'             = (f - k) / straddlePremium
      nu              = max (-1 + epsilon) (min nu' (1 - epsilon))
      eta             | abs nu < sqrtEpsilon = 1
                      | otherwise            = nu / (atanh nu)
      heta            = h eta
  in Vol $ sqrt (pi / (2 * t)) * straddlePremium * heta


euImpliedVolWith RootFinding cp (Forward forward) k t r (Premium p) =
  let root = ridders def (epsilon, 5*forward) f
      f vol = p' - p where (Premium p') = vPremium $ euOption b cp k t
                           b            = Bachelier (Forward forward) r (Vol vol)
  in case root of (Root vol) -> Vol vol
                  NotBracketed -> error "not bracketed"
                  SearchFailed -> error "search failed"

sqrtEpsilon = sqrt epsilon      
h eta = sqrt(eta) * (num / den) where
  a0          = 3.994961687345134e-1
  a1          = 2.100960795068497e+1
  a2          = 4.980340217855084e+1
  a3          = 5.988761102690991e+2
  a4          = 1.848489695437094e+3
  a5          = 6.106322407867059e+3
  a6          = 2.493415285349361e+4
  a7          = 1.266458051348246e+4

  b0          = 1.000000000000000e+0
  b1          = 4.990534153589422e+1
  b2          = 3.093573936743112e+1
  b3          = 1.495105008310999e+3
  b4          = 1.323614537899738e+3
  b5          = 1.598919697679745e+4
  b6          = 2.392008891720782e+4
  b7          = 3.608817108375034e+3
  b8          = -2.067719486400926e+2
  b9          = 1.174240599306013e+1

  num = a0 + eta * (a1 + eta * (a2 + eta * (a3 + eta * (a4 + eta * (a5 + eta * (a6 + eta * a7))))))
  den = b0 + eta * (b1 + eta * (b2 + eta * (b3 + eta * (b4 + eta * (b5 + eta * (b6 + eta * (b7 + eta * (b8 + eta * b9))))))))



f = Forward 100
vol = Vol 10
k = Strike 100
t = YearFrac 1
r = Rate 0
b = Bachelier f r vol
p = vPremium $ eucall b k t
vol' = euImpliedVol Call f k t r p
