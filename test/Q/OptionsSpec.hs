module Q.OptionsSpec where
import Q.Options

import Test.Hspec
import           Control.Monad (unless)
import Test.Hspec.Expectations

closeTo x y =  compareWith (\x y -> (abs $ (x - y)) <= 1e-5) errorMessage x y where
  errorMessage = "Is not close to"
  compareWith :: (HasCallStack, Show a) => (a -> a -> Bool) -> String -> a -> a -> Expectation
  compareWith comparator errorDesc result expected  = expectTrue errorMsg (comparator expected result)
    where errorMsg = show result ++ " " ++ errorDesc ++ " " ++ show expected
  expectTrue msg b = unless b (expectationFailure msg)

testHasTimeValue = hspec $ do
  describe "Time value tests " $ do
    let df = 0.9900498337491681
        f = 100
        k = 84
        p = 15.84079733998669
        cp = Call

    it "" $ do
      hasTimeValue cp f k p df `shouldBe` True

testIntrinisc  = undefined
testSuite = do
  -- Call, Strike 85.0, Premium 15.000000000000009
  let df = 1
      f = 100
      k = 85
      p = 15.000000000000009
      cp = Call
  print $ hasTimeValue cp f k p df
  testHasTimeValue
  testIntrinisc
