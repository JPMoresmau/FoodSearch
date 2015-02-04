{-# LANGUAGE FlexibleContexts #-}
-- | Use a neural network to perform the search for food
module AI.FoodSearch.Neural where

import AI.FoodSearch.Base

import           Network.Layer
import           Network.Neuron

import           Network.Network

import           Network.Trainer
import System.Random
import Numeric.LinearAlgebra.Data
import Numeric.Container

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
formatInputs w =   fromList . map (\i-> fromIntegral (snd i) / fromIntegral (wSmell w))   

-- | The definition of each layer
layerDef :: LayerDefinition Float
layerDef = LayerDefinition sigmoidNeuron dirLength connectFully

-- | The definition for all layers
allLayersDef :: [LayerDefinition Float]
allLayersDef = replicate 3 layerDef

-- | Create the network
buildNetwork :: RandomGen g => g -> Network Float
buildNetwork g = createNetwork normals g allLayersDef


-- | Train a network on several given worlds
train :: Network Float -> [World] -> Network Float
train n ws = 
  let t = BackpropTrainer (3 :: Float) quadraticCost quadraticCost'
      dat = concatMap trainData ws
  in trainUntilErrorLessThan n t online dat 0.01


-- | Use the network to give the answer 
neuralAlg ::  World -> Network Float -> StepFunction
neuralAlg w n _ is = toEnum $ maxIndex $ predict (formatInputs w is) n 

-- | Convert a network for only vectors structures
toVectors :: (Floating (Vector a), Container Vector a, Floating a) => 
  Network a -> [([Vector a],Vector a)]
toVectors = map (tovs . layerToShowable) . layers
  where
    tovs (ShowableLayer ws bs) = (toRows ws,bs)

-- | Convert back to a network given a network definition and vector structures
fromVectors :: (Floating (Vector a), Container Vector a, Floating a) => 
  LayerDefinition a -> [([Vector a],Vector a)] -> Network a 
fromVectors ld = Network . map ((\sl-> showableToLayer (sl,ld)) . frvs)
  where
    frvs (vs,v)=ShowableLayer (fromRows vs) v
