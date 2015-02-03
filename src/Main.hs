-- | Main entry point
module Main where

import Text.PrettyPrint
import System.Random
import AI.GeneticAlgorithm.Simple

import AI.FoodSearch.Base
import AI.FoodSearch.Genetic
import AI.FoodSearch.Neural
import AI.FoodSearch.Pretty


-- | entry function
main::IO()
main = do
  let sz = (10,10)
      pos = (5,6)
      w = buildWorld sz pos
      n = buildNetwork (mkStdGen 4)
      trained = train n [w]
      sz2 = (8,10)
      pos2 = (3,3)
      w2 = buildWorld sz2 pos2
      start = (7,7)
      s0 = algSteps w2 (neuralAlg w2 trained) 10 start
  putStrLn $ render $ pprPath w2 s0
  r@(NetworkData vxs _) <-runGAIO 64 0.1 (buildNetworkData [w,w2]) stopf
  print $ fitness r
  let trainedGA = fromVectors layerDef vxs
      s1 = algSteps w2 (neuralAlg w2 trainedGA) 10 start
  putStrLn $ render $ pprPath w2 s1
  
