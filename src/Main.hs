-- | Main entry point
module Main where

import Text.PrettyPrint
import System.Random

import AI.FoodSearch.Base
import AI.FoodSearch.Neural
import AI.FoodSearch.Pretty


-- | entry function
main::IO()
main = do
  let sz = (10,10)
      sm = 5
      pos = (5,6)
      w = buildWorld sz pos sm
      n = buildNetwork (mkStdGen 4)
      trained = train n [w]
      sz2 = (8,10)
      sm2 = 5
      pos2 = (3,3)
      w2 = buildWorld sz2 pos2 sm2
      start = (7,7)
      s0 = algSteps w2 (neuralAlg w2 trained) 10 start
  mapM_ (\(nb,st)->do
    putStrLn $ "Iteration " ++ show nb
    putStrLn $ render $ pprPosition w2 st
    putStrLn "") $ zip [1..] s0
--  print s0
--  print $ map (\p->(p,currentSmell w p)) $ allPositions w
--  print $ algStepExplain w baseAlg (0,1)
--  let (_,(is,dir)) = algStepExplain w baseAlg (0,1)
--  print $ formatInputs w is
--  print $ trainData w