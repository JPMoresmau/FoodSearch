module AI.FoodSearch.Base where

import Data.List (maximumBy)
import Data.Maybe
import Data.Ord

import qualified Data.Map as DM

type Direction = Int
type Smell = Int

type Input = [(Direction,Smell)]

type Position = (Int,Int)
type Size = (Int,Int)

type StepFunction = Position -> Input -> Direction

data World = World
    { wSize   :: Size
    , wSmell  :: Smell
    , wSmells :: DM.Map Position Smell
    }
    deriving (Show,Read,Eq,Ord)

baseAlg :: StepFunction
baseAlg _ = fst . maximumBy (comparing snd)


algStep :: World -> StepFunction -> Position -> Position
algStep w f p = fst $ algStepExplain w f p

algStepExplain :: World -> StepFunction -> Position -> (Position,([(Direction,Smell)],Direction))
algStepExplain w f p = let
  allSmells = map (\n->(n,fromMaybe 0 $ DM.lookup n $ wSmells w)) $ allNeighbours p
  posWithSmells = zip [0..] $ map snd allSmells
  dir = f p posWithSmells
  in (map fst allSmells !! dir,(posWithSmells,dir))

algSteps :: World -> StepFunction -> Int -> Position -> Position
algSteps w f maxIt p 
  | foundFood w p = p
  | maxIt == 0    = p
  | otherwise     = algSteps w f (maxIt-1) $ algStep w f p


currentSmell :: World -> Position -> Smell
currentSmell w p = 
  fromMaybe 
    (error $ show p ++ " is an invalid position") 
    (DM.lookup p $ wSmells w)


foundFood ::  World -> Position -> Bool
foundFood w p = wSmell w == currentSmell w p


buildWorld :: Size -> Position -> Smell -> World
buildWorld sz pos smell = World sz smell smells
    where 
      smells = waft [(pos,smell)] $ DM.singleton pos smell
      waft [] dm  = dm
      waft ((p,sm):rest) dm = 
           let ns  = filter (`DM.notMember` dm) $ neighbours p sz
               sm2 = sm - 1
               dm2 = foldr (`DM.insert` sm2) dm ns
           in waft (rest++ map (\n->(n,sm2)) ns) dm2


neighbours :: Position -> Size -> [Position]
neighbours p (w,h) = filter ok $ allNeighbours p
  where
    ok (a,b) = a>=0 && a < w && b>=0 && b < h && (a,b) /= p


allNeighbours :: Position -> [Position]
allNeighbours (x,y) = [(a,b) | a <- [x-1,x,x+1]
                                , b <- [y-1,y,y+1]]

allPositions :: World -> [Position]
allPositions = DM.keys . wSmells
