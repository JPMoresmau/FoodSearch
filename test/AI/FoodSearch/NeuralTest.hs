-- | Test neural network
module AI.FoodSearch.NeuralTest where

import AI.FoodSearch.Base
import AI.FoodSearch.Neural

import Test.Tasty
import Test.Tasty.HUnit
import System.Random

-- | Test neural network algorithm
neuralTests :: TestTree
neuralTests = do
  let sz = (5,5)
      sm = 3
      pos = (2,2)
      w = buildWorld sz pos sm
      trained = train w (mkStdGen 4)
  testGroup "NeuralTests" 
    [ testCase "Same World" $ do
        let s0 = algSteps w (neuralAlg w trained) 10 (0,0)
        s0 @?= [(0,0),(1,1),(2,2)]
    , testCase "Other World, same size" $ do
        let pos2 = (3,3)
            w2 = buildWorld sz pos2 4
            s0 = algSteps w2 (neuralAlg w2 trained) 10 (0,0)
        s0 @?= [(0,0),(1,1),(2,2),(3,3)]
    , testCase "Other World" $ do
        let sz2 = (8,10)
            sm2 = 8
            pos2 = (3,3)
            w2 = buildWorld sz2 pos2 sm2
            s0 = algSteps w2 (neuralAlg w2 trained) 10 (7,7)
        print w2
        -- he this fails, our network is overfitted
        s0 @?= [(7,7),(6,7),(6,6),(5,5),(4,4),(3,3)]
    ]