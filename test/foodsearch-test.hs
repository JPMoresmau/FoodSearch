-- | Test entry point
module Main where
import AI.FoodSearch.BaseTest


import Test.Tasty
import AI.FoodSearch.NeuralTest

-- | Main entry point
main :: IO()
main = defaultMain tests

-- | Test definitions
tests :: TestTree
tests = testGroup "Tests" [baseTests,neuralTests]
