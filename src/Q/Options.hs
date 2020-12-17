{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
module Q.Options
  (
    module Q.Types
  , Valuation(..)
  , intrinsinc
  , hasTimeValue
  ) where


import           Numeric.IEEE
import           Q.Types

data Valuation a = Valuation {
    vPremium :: Premium a
  , vDelta   :: Delta a
  , vVega    :: Vega a
  , vGamma   :: Gamma a
} deriving (Show)


-- | intrinsinc value of an option.
intrinsinc :: (forall a. Floating a, Ord a) => OptionType -> (Forward a) -> (Strike a) -> (DF a) -> (Premium a)
intrinsinc Call f (Strike k) df = pure $ max (s - k) 0
  where (Forward s) = df `discount` f
intrinsinc Put  f (Strike k) df = pure $ max (k - s) 0
  where (Forward s) = df `discount` f

-- | returns True if the undiscounted option premium is greater
--   than the undiscounted 'intrinsinc' (F - K)
hasTimeValue :: (Floating a, Ord a) =>
     OptionType
  -> (Forward a)
  -> (Strike a)
  -> (Premium a)
  -> (DF a)
  -> Bool
hasTimeValue cp f k p df = (intrinsinc cp f k df) < (df `undiscount` p)
