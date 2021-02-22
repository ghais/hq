{-# LANGUAGE FlexibleContexts #-}
module Q.SortedVector
  (
    fromList
  , fromVector
  , fromSortedList
  , SortedVector(..)
  , minElement
  , maxElement
  ) where

import           Data.Vector.Storable  (Storable)
import qualified Data.Vector.Storable as V (Vector (..), fromList, length, head, last)
import qualified Numeric.LinearAlgebra as V (sortVector)
import           Q.Types

newtype SortedVector a = SortedVector (V.Vector a)

fromList as = SortedVector (V.sortVector $ V.fromList as)
fromVector v = SortedVector (V.sortVector v)
fromSortedList xs = SortedVector $ V.fromList xs


minElement (SortedVector v) = V.head v
maxElement (SortedVector v) = V.last v
