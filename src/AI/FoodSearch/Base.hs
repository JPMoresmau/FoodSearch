-- | Base types and functions
module AI.FoodSearch.Base where

import Data.List (maximumBy)
import Data.Maybe
import Data.Ord

import qualified Data.Map as DM

-- | Direction to go to
data Direction = TopLeft | Left | BottomLeft | Top | Center | Bottom | TopRight | Right | BottomRight
  deriving (Show,Read,Eq,Ord,Bounded,Enum)

-- | Strength of the smell
type Smell = Int
-- | Input information
type Input = [(Direction,Smell)]
-- | Position in world
type Position = (Int,Int)
-- | Size of the world
type Size = (Int,Int)
-- | Maximum number of steps to take
type Max = Int

-- | Number of directions
dirLength :: Int
dirLength = 1 + fromEnum (maxBound :: Direction)


-- | Function deciding in which direction to move
type StepFunction = Position -> Input -> Direction

-- | The world
data World = World
    { wSize   :: Size -- ^ size
    , wFood   :: Position -- ^ Position of the food
    , wSmell  :: Smell -- ^ Smell of the food position
    , wSmells :: DM.Map Position Smell -- ^ All smell strengths by position
    }
    deriving (Show,Read,Eq,Ord)

-- | The base algorithm: just go toward the highest smell
baseAlg :: StepFunction
baseAlg _ = fst . maximumBy (comparing snd)

-- | Perform one step of the algorithm
algStep :: World -> StepFunction -> Position -> Position
algStep w f p = fst $ algStepExplain w f p

-- | Perform one step and return the information generated: direction/smell input, direction output
algStepExplain :: World -> StepFunction -> Position -> (Position,([(Direction,Smell)],Direction))
algStepExplain w f p = let
  allSmells = map (\n->(n,fromMaybe 0 $ DM.lookup n $ wSmells w)) $ allNeighbours p
  posWithSmells = zip [minBound..] $ map snd allSmells
  dir = f p posWithSmells
  newPos = map fst allSmells !! fromEnum dir
  in if isValid w newPos -- defensive
    then (newPos,(posWithSmells,dir))
    else (p,(posWithSmells,Center))

-- | Perform all algorithm steps till arriving at food or max steps
algSteps :: World -> StepFunction -> Max -> Position -> [Position]
algSteps w f maxIt p = reverse $ go maxIt [p]
  where
    go _ [] = error "garg"
    go m ps@(p2:_)
      | foundFood w p2 = ps
      | m == 0         = ps
      | otherwise      = go (m-1) (algStep w f p2 : ps)

-- | is the position valid?
isValid ::  World -> Position -> Bool
isValid w p =p `DM.member` wSmells w

-- | Smell of the given position
currentSmell :: World -> Position -> Smell
currentSmell w p = 
  fromMaybe 
    (error $ show p ++ " is an invalid position") 
    (DM.lookup p $ wSmells w)

-- | Have we found food?
foundFood ::  World -> Position -> Bool
foundFood w p = wFood w == p

-- | Build the world of the given size, with the food at the given position
buildWorld :: Size -> Position -> World
buildWorld sz pos = World sz pos maxSmell smells
    where 
      maxSmell = 1 + maximum (map (distance pos) $ corners sz)
      smells = waft [(pos,maxSmell)] $ DM.singleton pos maxSmell
      waft [] dm  = dm
      waft ((p,sm):rest) dm = 
           let ns  = filter (`DM.notMember` dm) $ neighbours p sz
               sm2 = sm - 1
               dm2 = foldr (`DM.insert` sm2) dm ns
           in waft (rest++ map (\n->(n,sm2)) ns) dm2


-- | Distance between two positions.
distance :: Position -> Position -> Int
distance (a,b) (c,d) = maximum [abs $ a - c, abs $ b - d]

-- | the corners of a world
corners :: Size -> [Position]
corners sz = [(x,y) | x<-[0,fst sz - 1],y<-[0,snd sz - 1]]

-- | Get reachable neighbours of the given position
neighbours :: Position -> Size -> [Position]
neighbours p (w,h) = filter ok $ allNeighbours p
  where
    ok (a,b) = a>=0 && a < w && b>=0 && b < h && (a,b) /= p

-- | Get all neighbours, even if outside world or given position itself
allNeighbours :: Position -> [Position]
allNeighbours (x,y) = [(a,b) | a <- [x-1,x,x+1]
                                , b <- [y-1,y,y+1]]

-- | Get all positions in the world
allPositions :: World -> [Position]
allPositions = DM.keys . wSmells
