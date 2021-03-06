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
      pos = (2,2)
      w = buildWorld sz pos
      n = buildNetwork (mkStdGen 4)
      sz2 = (15,20)
      pos2 = (5,10)
      w2 = buildWorld sz2 pos2
      trained = train n [w,w2] 
      trained2 = fromVectors layerDef $ toVectors trained
  testGroup "NeuralTests" 
    [ testCase "Same World" $ do
        let s0 = algSteps w (neuralAlg w trained) 10 (0,0)
        let s1 = algSteps w (neuralAlg w trained) 10 (4,4)
        s0 @?= [(0,0),(1,1),(2,2)]
        s1 @?= [(4,4),(3,3),(2,2)]
    , testCase "Other World, same size" $ do
        let pos3 = (3,3)
            w3 = buildWorld sz pos3
            s0 = algSteps w3 (neuralAlg w3 trained) 10 (0,0)
            s1 = algSteps w3 (neuralAlg w3 trained) 10 (4,4)
        s0 @?= [(0,0),(1,1),(2,2),(3,3)]
        s1 @?= [(4,4),(3,3)]
    , testCase "Other World" $ do
        let sz3 = (8,10)
            pos3 = (3,3)
            w3 = buildWorld sz3 pos3
            s0 = algSteps w3 (neuralAlg w3 trained) 10 (7,7)
        s0 @?= [(7,7),(6,7),(6,6),(5,5),(4,4),(3,3)]
    , testCase "Other World after vector transformation" $ do
        let sz3 = (8,10)
            pos3 = (3,3)
            w3 = buildWorld sz3 pos3
            s0 = algSteps w3 (neuralAlg w3 trained2) 10 (7,7)
        let tv= toVectors trained
        print $ length tv
        print $ map (length . fst) tv
        s0 @?= [(7,7),(6,7),(6,6),(5,5),(4,4),(3,3)]
    ]