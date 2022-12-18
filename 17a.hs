import Data.Maybe (catMaybes, isJust)
import qualified Data.Map as Map
import Control.Monad.Writer

data Instruction = L | R deriving (Show)
type Coords = (Int, Int)
type Rocks = Map.Map Coords Bool
type Shape = [Coords]
type CacheMap = Map.Map Int Int

shapes :: [[Coords]]
shapes = [[(0, 0), (1, 0), (2, 0), (3, 0)], [(1, 0), (0,1), (1, 1), (2, 1), (1,2)], [(2, 0), (2, 1), (0, 0), (1, 0), (2, 2)], [(0, 0), (0, 1), (0, 2), (0, 3)], [(0, 0), (1, 0), (0, 1), (1, 1)]]

mapContent :: Char -> Maybe Instruction
mapContent '>' = Just R
mapContent '<' = Just L
mapContent _ = Nothing

getDirection :: Instruction -> Coords
getDirection L = (-1, 0)
getDirection R = (1, 0)

sumCoords :: Coords -> Coords -> Coords
sumCoords (a, b) (c, d) = (a + c, b + d)

insertShape :: Rocks -> [Coords] -> Rocks
insertShape r s = foldl (\r_ s_ -> Map.insert s_ True r_) r s

towerHeight :: Rocks -> Int
towerHeight r = (foldl max (-1) $ map snd $ Map.keys r) + 1

startingCoords :: Rocks -> Coords
startingCoords r = (2, towerHeight r + 3)

hasCollision :: Rocks -> Shape -> Bool
hasCollision rocks shape = any (\s@(x,y) -> y < 0 || x < 0 || x > 6 || (isJust $ Map.lookup s rocks)) shape

applyInstruction :: Rocks -> (Int, Instruction) -> Shape -> (Shape, Bool)
applyInstruction r (_,i) s = newShape2
    where moved1 = map (sumCoords (getDirection i)) s
          newShape = if (hasCollision r moved1) then s else moved1
          moved2 = map (sumCoords (0, -1)) newShape
          newShape2 = if (hasCollision r moved2) then (newShape, True) else (moved2, False)

applyInstructions :: Rocks -> ([(Int, Instruction)], Shape) -> ([(Int, Instruction)], Shape)
applyInstructions r ((i:is), s) = if finished then (is, newS) else (applyInstructions r (is, newS))
    where (newS, finished) = applyInstruction r i s

loop :: ([(Int, Instruction)], Rocks, Int) -> Writer CacheMap ([(Int, Instruction)], Rocks, Int)
loop (startingInstructions, r, sID) = do
    tell $ Map.fromList [(fst $ head startingInstructions, towerHeight r)]
    return (instructions, newR, (sID+1) `rem` 5)
    where (c, d) = startingCoords r
          startingShape = map (\(a, b) -> (a + c, b + d)) (shapes !! sID)
          (instructions, shape) = applyInstructions r (startingInstructions, startingShape)
          newR = insertShape r shape

renderMap :: ([Instruction], Rocks, Int) -> String
renderMap (i, r, _) = (concat $ map (\y -> (map (\x -> if (isJust $ Map.lookup (x,y) r) then '#' else '.') [0..6]) ++ "\n") [30,29..0]) ++ "\n"

main = do
    content <- readFile "17.txt"
    let instructions = cycle $ zip [0..] $ catMaybes $ map mapContent content

    let (gameState, log) = runWriter $ head $ drop roundsMod $ iterate (>>= loop) (return (instructions, Map.empty, 0))
    putStrLn $ show $ extraHeight + (\(_, x, _) -> towerHeight x) gameState

    where rounds = 1000000000000
          -- Tower gets 2613 units higher in every 1715 rounds.
          -- It also has a pre-period, therefore just taking mod is not enough.
          -- How to do this generally for any input? TODO
          roundsMod = (rounds `rem` 1715) + 1715
          extraHeight = ((rounds `div` 1715) - 1) * 2613