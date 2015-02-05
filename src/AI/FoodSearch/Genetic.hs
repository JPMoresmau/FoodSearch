-- | Genetic algorithm to evolve the neural network
module AI.FoodSearch.Genetic where

import Control.Monad.Random

import AI.GeneticAlgorithm.Simple
import qualified Data.Vector.Storable as V
import Numeric.Container
import Control.DeepSeq

import AI.FoodSearch.Base
import AI.FoodSearch.Neural

-- | Store the network information as vectors
data NetworkData = NetworkData [([Vector Float],Vector Float)] [World]
  deriving (Show,Read,Eq,Ord)

-- | Need to be able to rnf
instance NFData NetworkData where
  rnf (NetworkData x _) = seq x ()

-- | Chromosome instance
instance Chromosome NetworkData where
    -- Take average
    crossover (NetworkData xs ws) (NetworkData ys _) =
        return [ NetworkData (Prelude.zipWith zipW xs ys) ws]
        where
          zipW (vs1,v1) (vs2,v2) = (Prelude.zipWith zipW1 vs1 vs2,zipW1 v1 v2)
          zipW1 = V.zipWith (\x y -> (x + y) / 2) 

    -- Mutate one weight randomly
    mutation (NetworkData xs ws) = do
        xs' <- randomChange r1 xs
        return $ NetworkData xs' ws
      where
        randomChange _ [] =  return []
        randomChange f xs2 = do
          idx <- getRandomR (0, length xs2 - 1)
          mapM (\(i,x)->if i==idx then f x else return x) $ zip [0..] xs2
        r1 (vs,v) = do
          (v3:vs2) <- randomChange r2 (v:vs)
          return (vs2,v3)
        r2 v2 = do
          idx2 <- getRandomR (0, V.length v2 - 1)
          dx   <- getRandomR (-20, 20)  
          return $ v2  V.// [(idx2,dx)]         
 
    -- calculate fitness
    fitness (NetworkData xs ws) = sum (map (fitWorld xs) ws) / fromIntegral (length ws)
 
 
 
-- | Calculate fitness on a given world   
fitWorld :: [([Vector Float],Vector Float)] -> World -> Double
fitWorld dat w = sum (map fitCorner $ corners $ wSize w) / 4
  where
    fitCorner pos = 
      let network = fromVectors layerDef dat
          poss = algSteps w (neuralAlg w network) 50 pos
          endSmell = currentSmell w $ last poss
          possLength = length poss
      in fitFormula w endSmell possLength (distance pos $ wFood w)
      
-- | Fitness formula
fitFormula :: World -> Int -> Int -> Int -> Double
fitFormula w endSmell possLenth dist = case fromIntegral endSmell / fromIntegral (wSmell w) of
    1 -> 2 + (fromIntegral dist / fromIntegral possLenth)
    n -> n

-- | Maximum for the fitness
maxFit :: Double
maxFit = fitFormula (World undefined undefined 10 undefined) 10 10 10

-- | Stop function
stopf ::  NetworkData -> Int -> IO Bool
stopf nd gen= return $ gen > 300 || fitness nd == maxFit

-- | Build a random network data
buildNetworkData :: (Monad m,RandomGen g) => [World] -> RandT g m NetworkData 
buildNetworkData ws= do
  g <- getSplit
  let n = buildNetwork g
  return $ NetworkData (toVectors n) ws

