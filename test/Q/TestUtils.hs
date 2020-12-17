module Q.TestUtils where

import Test.Hspec.Expectations
import           Control.Monad (unless, guard, when)

closeTo x y tolerance =  compareWith (\x y -> (abs $ (x - y)) <= tolerance) errorMessage x y where
  errorMessage = "Is not close to"
  compareWith :: (HasCallStack, Show a) => (a -> a -> Bool) -> String -> a -> a -> Expectation
  compareWith comparator errorDesc result expected  = expectTrue errorMsg (comparator expected result)
    where errorMsg = show result ++ " " ++ errorDesc ++ " " ++ show expected
  expectTrue msg b = unless b (expectationFailure msg)
