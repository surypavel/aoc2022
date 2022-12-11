import Control.Monad.Writer
import qualified Data.Map as Map
import Data.Maybe (catMaybes)

type Coords = (Int, Int)
type Memory = Map.Map Coords Bool;
type State = [Coords]

sumCoords :: Coords -> Coords -> Coords
sumCoords (a,b) (c,d) = (a+c, b+d)

moveCoords :: Coords -> Coords -> Coords
moveCoords (a,b) (c,d) = (signum $ a-c, signum $ b-d)

detachedCoords :: Coords -> Coords -> Bool
detachedCoords (a, b) (c, d) = abs (a - c) > 1 || abs (b - d) > 1

getInstruction :: [String] -> [Coords]
getInstruction (x:y:xs) | x == "D" = replicate (read y) (0, 1)
getInstruction (x:y:xs) | x == "U" = replicate (read y) (0, -1)
getInstruction (x:y:xs) | x == "L" = replicate (read y) (-1, 0)
getInstruction (x:y:xs) | x == "R" = replicate (read y) (1, 0)
getInstruction _ = []

getInstructions :: String -> [Coords]
getInstructions content = foldl (++) [] $ map (getInstruction . words) (lines content)

moveKnot :: State -> Coords -> Writer Memory State
moveKnot (x:y:xs) move = if detached
    then do
        newTail <- moveKnot (y:xs) (moveCoords newHead y)
        return $ newHead:newTail
    else do
        return $ newHead:y:xs
    where newHead = sumCoords x move
          detached = detachedCoords newHead y
moveKnot (x:xs) move = do
    tell $ Map.fromList [(newHead, True)]
    return [newHead]
    where newHead = sumCoords x move

moveKnots :: Int -> [Coords] -> Writer Memory State
moveKnots r coords = do 
    tell $ Map.fromList [((0, 0), True)]
    foldM moveKnot (replicate r (0,0)) coords

numberOfVisited x = length $ Map.keys $ snd $ runWriter x;

main :: IO ()
main = do
    content <- readFile "9.txt"
    let instructions = getInstructions content
    putStrLn $ show $ numberOfVisited $ moveKnots 2 instructions
    putStrLn $ show $ numberOfVisited $ moveKnots 10 instructions
