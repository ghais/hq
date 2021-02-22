{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module Q.Greeks
  (
      module Q.Types
    , module Q.Options
  )
where

import Q.Types
import Q.Options
import Data.Coerce

-- | A relative or an absolute bump. Used with numerical Greeks.
data Bump = Abs Double
          | Rel Double

data DiffMethod = ForwardDiff
                | BackwardDiff
                | CenteralDiff

class Bumpable a where
  bumpUp   :: Bump -> a -> a
  bumpDown :: Bump -> a -> a
  stepSize :: Bump -> a -> Double

-- | Things we can bump to calculate Greeks such as 'Spot', 'Rate'..etc'
instance (Coercible a Double) => Bumpable a where
  bumpUp (Abs bump) a = coerce $ coerce a + bump
  bumpUp (Rel bump) a = coerce $ coerce a * (1 + bump)

  bumpDown (Abs bump) a = coerce $ coerce a - bump
  bumpDown (Rel bump) a = coerce $ coerce a * (1 - bump)

  stepSize (Abs bump) _ = bump
  stepSize (Rel bump) s = coerce s * bump



firstOrder :: (Bumpable a) => DiffMethod -> Bump -> (a -> Double) -> a -> Double
firstOrder ForwardDiff b f a =  df / dx
  where df = f a' - f a
        a' = bumpUp b a
        dx = stepSize b a :: Double

firstOrder BackwardDiff d f a = df / dx
   where df = f a - f a'
         a' = bumpDown d a
         dx = negate (stepSize d a)

firstOrder CenteralDiff b f a = df / dx
   where df = f u - f d
         u = bumpUp b a
         d = bumpDown b a
         dx = 2 * stepSize b a

