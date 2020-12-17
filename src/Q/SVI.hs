{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
module Q.SVI where
import           Q.Types      (Forward (..), Strike (..), Vol (..),
                               YearFrac (..))

import           GHC.Generics (Generic)

newtype Alpha  = Alpha  Double deriving (Generic, Eq, Show, Ord, Num, Fractional, Floating)
newtype Beta   = Beta   Double deriving (Generic, Eq, Show, Ord, Num, Fractional, Floating)
newtype Rho    = Rho    Double deriving (Generic, Eq, Show, Ord, Num, Fractional, Floating)
newtype M      = M      Double deriving (Generic, Eq, Show, Ord, Num, Fractional, Floating)
newtype Sigma  = Sigma  Double deriving (Generic, Eq, Show, Ord, Num, Fractional, Floating)

-- | Stochastic volatility inspired parameterization of the vol surface.
data SVI = RSVI     -- ^ The original raw SVI representation from Gatheral
           Alpha   -- ^ Corresponds to a vertical translation of the smile.
           Beta    -- ^ Slope of call and put wings.
           Rho     -- ^ A counter clock wise rotation of the smile.
           M       -- ^ translate the smile to the right
           Sigma   -- ^ ATM curviture of the smile.

newtype LogRelativeStrike = LogRelativeStrike Double
  deriving (Generic, Eq, Show, Ord, Num, Fractional, Floating)

-- | A volatility slice
class Smile a where
  totalVar :: a -> LogRelativeStrike -> YearFrac -> Double

  vol      :: a -> LogRelativeStrike -> YearFrac -> Double
  vol a k t@(YearFrac t')  = sqrt (totalVar a k t) / t'


instance Smile SVI where
  totalVar (RSVI (Alpha 𝜶) (Beta 𝜷) (Rho 𝛒) (M 𝐦) (Sigma 𝛔)) (LogRelativeStrike 𝐤) _ =
    𝜶 + 𝜷 * (𝛒 * (𝐤 - 𝐦) + sqrt ((𝐤 - 𝐦) ** 2 + 𝛔 * 𝛔))

instance Smile (Vol Double) where
  totalVar (Vol 𝛔) _ (YearFrac t)  = 𝛔 * 𝛔 * t
