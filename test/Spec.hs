module Main where
import Q.Types
import Q.Options
import Data.Coerce (coerce)
import Q.BlackScholes

import Q.ImpliedVol.LetsBeRational
main :: IO ()
main = do
  let f = Spot 100
      cp = Call
      t = YearFrac 1
      r = Rate 0.0
      vol = Vol 0.1
      bs = BlackScholes f r vol
      k = atmf bs t
      p = vPremium $ eucall bs k t
      vol' = euImpliedVol Call (Forward 100) k t r p
  print vol'
