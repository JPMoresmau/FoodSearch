-- | Use a neural network to perform the search for food
module AI.FoodSearch.Neural where

import AI.FoodSearch.Base

import           Network.Layer
import           Network.Neuron

import           Network.Network

import           Network.Trainer
import System.Random
import Numeric.LinearAlgebra.Data

-- | Training data: for each position in the world, use the base algorithm to get the training answer
trainData ::  World -> [(Vector Float, Vector Float)]
trainData w = map onePos $ allPositions w
  where
    onePos p = 
      let (_,(is,dir)) = algStepExplain w baseAlg p
          os = map (\(d,_)->if dir==d then 1 else 0) is 
      in (formatInputs w is,fromList os)
      
-- | Format the inputs suitable for the network
formatInputs ::  World -> [(Direction,Smell)] ->  Vector Float
formatInputs  w =   fromList . map (\i-> fromIntegral (snd i) / fromIntegral (wSmell w))   


-- | Train a network on the given world
train :: RandomGen g => World -> g -> Network Float
train w g = 
  let l = LayerDefinition sigmoidNeuron dirLength connectFully
      l' = LayerDefinition sigmoidNeuron dirLength connectFully
      l'' = LayerDefinition sigmoidNeuron dirLength connectFully
      n = createNetwork normals g [l, l', l'']
      t = BackpropTrainer (3 :: Float) quadraticCost quadraticCost'
      dat = trainData w
  in trainUntilErrorLessThan n t online dat 0.01

-- | Use the network to give the answer 
neuralAlg ::  World -> Network Float -> StepFunction
neuralAlg w n _ is = toEnum $ maxIndex $ predict (formatInputs w is) n 
