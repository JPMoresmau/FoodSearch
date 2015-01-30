module Main where

import System.Random
import AI.FoodSearch.Base
import AI.FoodSearch.Neural

main::IO()
main = do
  let sz = (5,5)
      sm = 3
      pos = (2,2)
      w = buildWorld sz pos sm
      trained = train w (mkStdGen 4)
      s0 = algSteps w (neuralAlg w trained) 10 (0,0)
  print s0
--  print $ map (\p->(p,currentSmell w p)) $ allPositions w
--  print $ algStepExplain w baseAlg (0,1)
--  let (_,(is,dir)) = algStepExplain w baseAlg (0,1)
--  print $ formatInputs w is
--  print $ trainData w