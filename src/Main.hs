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
      trained = train w (mkStdGen 4)
      start = (2,0)
      s0 = algSteps w (neuralAlg w trained) 10 start
  mapM_ (\(nb,st)->do
    putStrLn $ "Iteration " ++ show nb
    putStrLn $ render $ pprPosition w st
    putStrLn "") $ zip [1..] s0
--  print s0
--  print $ map (\p->(p,currentSmell w p)) $ allPositions w
--  print $ algStepExplain w baseAlg (0,1)
--  let (_,(is,dir)) = algStepExplain w baseAlg (0,1)
--  print $ formatInputs w is
--  print $ trainData w