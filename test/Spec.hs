module Main where
import Q.BachelierSpec
import Q.ImpliedVol.NormalSpec
main :: IO ()
main = do
  bachelierTests
  normalImpliedVolTests
