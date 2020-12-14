module Q.ImpliedVol.NormalSpec(normalImpliedVolTests) where
import Test.Hspec hiding (shouldBe)
import Q.Bachelier
import Q.Types
import Test.Hspec.Expectations
import           Control.Monad (unless)
import Q.ImpliedVol.Normal

closeTo x y =  compareWith (\x y -> (abs $ (x - y)) <= 1e-6) errorMessage x y where
  errorMessage = "Is not close to"
  compareWith :: (HasCallStack, Show a) => (a -> a -> Bool) -> String -> a -> a -> Expectation
  compareWith comparator errorDesc result expected  = expectTrue errorMsg (comparator expected result)
    where errorMsg = show result ++ " " ++ errorDesc ++ " " ++ show expected
  expectTrue msg b = unless b (expectationFailure msg)


test cp (k, b@(Bachelier f r sigma)) = do
  let v = euOption b cp k t
      p      = vPremium v
      sigma' = euImpliedVol cp f k t r p
  it (show cp ++ ", " ++ show k ++ ", " ++ show p ++ " has an implied vol of " ++ show sigma) $ do
    sigma' `closeTo` sigma

runTests f r strikes vols = do
  let bs        = [Bachelier f r sigma | sigma <- vols]
      testCases = [(k, b)              | k <-  strikes, b <- bs]
  context "Call Option" $ do
    sequence_ (map (test Call) testCases)
  context "Put Option" $ do
    sequence_ (map (test Put) testCases)

normalImpliedVolTests = hspec $ do
  describe "bachelier european implied vol" $ do
    context "When asset price is positive ($100)" $ do
      let strikes   = [Strike k            | k <- [80,81..120]]
          vols      = [Vol sigma           | sigma <- [5,10..200]]
      let f = Forward 100
      context "1Y option" $ do
        let t = YearFrac 1                      
        context "When interest rate is zero (0%)" $ do
          let r         = Rate 0
          runTests f r strikes vols
        context "When interest rate is slightly positive (1%)" $ do
          let r         = Rate 0.01
          runTests f r strikes vols
        context "When interest rate is slightly negative (-1%)" $ do
          let r         = Rate (-0.01)
          runTests f r strikes vols
