import qualified Data.Map as Map
import Data.List (findIndex, minimumBy, maximumBy)
import Data.Maybe (fromJust, isJust)
import Data.Ord (comparing)
import Control.Applicative

type DirectionChange = Int
type Direction = Int
type Instruction = (Direction, DirectionChange)
type Coords = (Int,Int)
type Accessibility = Map.Map Coords Bool
type State = (Coords, Direction)

getNumber :: String -> (Int, String)
getNumber s = (read number, rest)
    where isNumber = flip elem ['0'..'9']
          number = takeWhile isNumber s
          rest = dropWhile isNumber s

getDirection :: Char -> DirectionChange
getDirection 'L' = -1
getDirection 'R' = 1

loadInstructions :: String -> [Instruction]
loadInstructions [] = []
loadInstructions (x:xs) = (d,n):(loadInstructions rest)
    where d = getDirection x
          (n, rest) = getNumber xs

loadInput :: String -> (Accessibility, String, Int)
loadInput s = (Map.fromList m, p, x)
    where l = lines s
          p = last l
          m = concatMap (\(i,line) -> map (\(j,c) -> ((j,i), c == '.')) $ filter ((/=' ') . snd) $ zip [1..] line) $ zip [1..] $ takeWhile (/="") l
          x = (+1) $ fromJust $ findIndex (=='.') $ head l

moveCoords :: Direction -> Coords -> Coords
moveCoords 0 (x, y) = (x+1,y)
moveCoords 1 (x, y) = (x,y+1)
moveCoords 2 (x, y) = (x-1,y)
moveCoords 3 (x, y) = (x,y-1)

getWrap :: Direction -> (Coords -> Int, [Coords] -> Coords)
getWrap 0 = (snd, minimumBy (comparing fst))
getWrap 1 = (fst, minimumBy (comparing snd))
getWrap 2 = (snd, maximumBy (comparing fst))
getWrap 3 = (fst, maximumBy (comparing snd))

moveInDirection :: Accessibility -> Direction -> Coords -> Coords
moveInDirection m direction coords = if findFinalCoords == Just True then finalCoords else coords
    where newCoords = moveCoords direction coords
          findNewCoords = Map.lookup newCoords m
          (f1, f2) = getWrap direction
          wrappedCoords = f2 $ filter ((==f1 coords) . f1) $ Map.keys m
          findWrappedCoords = Map.lookup wrappedCoords m
          finalCoords = if (isJust findNewCoords) then newCoords else wrappedCoords
          findFinalCoords = findNewCoords <|> findWrappedCoords

move :: Accessibility -> State -> Instruction -> State
move m (coords, direction) (directionChange, steps) = (newCoords, newDirection)
    where newDirection = (direction + directionChange) `mod` 4
          newCoords = (!! steps) $ iterate (moveInDirection m newDirection) coords

evalState :: State -> Int
evalState ((x,y),d) = 1000 * y + 4 * x + d

main = do
    content <- readFile "22.txt"
    let (m, path, x) = loadInput content
    let startingState = ((x, 1), 3)
    let instructions = loadInstructions ('R':path)
    putStrLn $ show $ evalState $ foldl (move m) startingState instructions
