module Q.ImpliedVol.Normal where
import           Q.Bachelier
import           Q.Types
import           Statistics.Distribution        (cumulative, density)
import           Statistics.Distribution.Normal (standard)

-- | Calcualte the bachelier option implied vol of a european option.
--
--
euImpliedVol :: OptionType -> Forward -> Strike -> YearFrac -> Rate -> Premium -> Vol
euImpliedVol cp (Forward f) (Strike k) (YearFrac t) (Rate r) (Premium p)
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
