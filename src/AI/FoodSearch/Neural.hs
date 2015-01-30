module AI.FoodSearch.Neural where

import AI.FoodSearch.Base

import           Network.Layer
import           Network.Neuron

import           Network.Network

import           Network.Trainer
import System.Random
import Numeric.LinearAlgebra.Data

trainData ::  World -> [(Vector Float, Vector Float)]
trainData w = map onePos $ allPositions w
  where
    onePos p = 
      let (_,(is,dir)) = algStepExplain w baseAlg p
          os = map (\(ix,_)->if dir==ix then 1 else 0) $ zip [0..] is 
      in (formatInputs w is,fromList os)
      
formatInputs ::  World -> [(Direction,Smell)] ->  Vector Float
formatInputs  w =   fromList . map (\i-> fromIntegral (snd i) / (fromIntegral $ wSmell w))   

      
posLength :: Int
posLength = 9

train :: RandomGen g => World -> g -> Network Float
train w g = 
  let l = LayerDefinition sigmoidNeuron posLength connectFully
      l' = LayerDefinition sigmoidNeuron posLength connectFully
      l'' = LayerDefinition sigmoidNeuron posLength connectFully
      n = createNetwork normals g [l, l', l'']
      t = BackpropTrainer (3 :: Float) quadraticCost quadraticCost'
      dat = trainData w
  in trainUntilErrorLessThan n t online dat 0.01


neuralAlg ::  World -> Network Float -> StepFunction
neuralAlg w n _ is = maxIndex $ predict (formatInputs w is) n 
