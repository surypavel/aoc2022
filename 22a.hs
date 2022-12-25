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
          m = concatMap (\(i,line) -> map (\(j,c) -> ((j,i), c == '.')) $ filter ((/=' ') . snd) $ zip [0..] line) $ zip [0..] $ takeWhile (/="") l
          x = fromJust $ findIndex (=='.') $ head l

moveCoords :: Direction -> Coords -> Coords
moveCoords 0 (x, y) = (x+1,y)
moveCoords 1 (x, y) = (x,y+1)
moveCoords 2 (x, y) = (x-1,y)
moveCoords 3 (x, y) = (x,y-1)

charToDirection :: Char -> Direction
charToDirection 'R' = 0
charToDirection 'D' = 1
charToDirection 'L' = 2
charToDirection 'U' = 3

directionToChar :: Direction -> Char
directionToChar 0 = 'R'
directionToChar 1 = 'D'
directionToChar 2 = 'L'
directionToChar 3 = 'U'

getWrap :: Direction -> (Coords -> Int, [Coords] -> Coords)
getWrap 0 = (snd, minimumBy (comparing fst))
getWrap 1 = (fst, minimumBy (comparing snd))
getWrap 2 = (snd, maximumBy (comparing fst))
getWrap 3 = (fst, maximumBy (comparing snd))

getWrappedCoords :: Accessibility -> (Direction, Coords) -> (Direction, Coords)
getWrappedCoords m (direction,coords) = (direction, f2 $ filter ((==f1 coords) . f1) $ Map.keys m)
    where (f1, f2) = getWrap direction

addPair :: Coords -> Coords -> Coords
addPair (a,b) (c,d) = (a+c, b+d)

multiplyPair :: Int -> Coords -> Coords
multiplyPair n (a,b) = (a*n, b*n)

transformCoords :: Direction -> Direction -> Coords -> Coords
transformCoords 0 0 (a,b) = (0,b)
transformCoords 0 1 (a,b) = (cubeSize-b-1,0)
transformCoords 0 2 (a,b) = (cubeSize-1,cubeSize-1-b)
transformCoords 0 3 (a,b) = (b,cubeSize-1)
transformCoords 1 0 (a,b) = (0,cubeSize-1-a)
transformCoords 1 1 (a,b) = (a,0)
transformCoords 1 2 (a,b) = (cubeSize-1,a)
transformCoords 1 3 (a,b) = (cubeSize-1-a,cubeSize-1)
transformCoords 2 0 (a,b) = (0,cubeSize-1-b)
transformCoords 2 1 (a,b) = (b,0)
transformCoords 2 2 (a,b) = (cubeSize-1,b)
transformCoords 2 3 (a,b) = (cubeSize-1-b,cubeSize-1)
transformCoords 3 0 (a,b) = (0,a)
transformCoords 3 1 (a,b) = (cubeSize-1-a,0)
transformCoords 3 2 (a,b) = (cubeSize-1,cubeSize-1-a)
transformCoords 3 3 (a,b) = (a,cubeSize-1)

getWrappedCoords2 :: Accessibility -> (Direction, Coords) -> (Direction, Coords)
getWrappedCoords2 m (direction,coords) = (newDirection, newCoords)
    where currentCube = getCubePart coords
          currentCubeCoords = ((fst coords) `mod` cubeSize, (snd coords) `mod` cubeSize)
          (newCube, newDirectionChar) = getCubeWrap (currentCube, directionToChar direction)
          newDirection = charToDirection newDirectionChar
          newCubeCoords = transformCoords direction newDirection currentCubeCoords
          newCoords = addPair (multiplyPair cubeSize $ cubeCoords !! (newCube-1)) newCubeCoords

moveInDirection :: Accessibility -> (Direction, Coords) -> (Direction, Coords)
moveInDirection m (direction, coords) = (newDirection, if findFinalCoords == Just True then finalCoords else coords)
    where newCoords = moveCoords direction coords
          findNewCoords = Map.lookup newCoords m
          (newDirection_, wrappedCoords) = getWrappedCoords2 m (direction,coords)
          findWrappedCoords = Map.lookup wrappedCoords m
          newDirection = if (isJust findNewCoords || findFinalCoords == Just False) then direction else newDirection_
          finalCoords = if (isJust findNewCoords) then newCoords else wrappedCoords
          findFinalCoords = findNewCoords <|> findWrappedCoords

move :: Accessibility -> State -> Instruction -> State
move m (coords, direction) (directionChange, steps) = (newCoords, newDirection2)
    where newDirection = (direction + directionChange) `mod` 4
          (newDirection2, newCoords) = (!! steps) $ iterate (\dc -> moveInDirection m dc) (newDirection, coords)

-- Count lines from 1, not from 0
evalState :: State -> Int
evalState ((x,y),d) = 1000 * (y+1) + 4 * (x+1) + d

-- Black box for determining the cube paritions

getCubePart :: Coords -> Int
getCubePart (a,b) = 1 + (fromJust $ findIndex (\(x,y) -> a >= x*cubeSize && a < (x+1)*cubeSize && b >= y*cubeSize && b < (y+1)*cubeSize) cubeCoords)

cubeSize = 50
cubeCoords = [(1, 0), (2, 0), (1, 1), (1, 2), (0, 2), (0, 3)]
cubeCoords_ = [(2, 0), (0, 1), (1, 1), (2, 1), (2, 2), (3, 2)]

-- Black box for cube wraps
getCubeWrap :: (Int, Char) -> (Int, Char)
getCubeWrap (1, 'D') = (3, 'D')
getCubeWrap (1, 'R') = (2, 'R')
getCubeWrap (1, 'U') = (6, 'R')
getCubeWrap (1, 'L') = (5, 'R')
getCubeWrap (2, 'D') = (3, 'L')
getCubeWrap (2, 'R') = (4, 'L')
getCubeWrap (2, 'U') = (6, 'U')
getCubeWrap (2, 'L') = (1, 'L')
getCubeWrap (3, 'D') = (4, 'D')
getCubeWrap (3, 'R') = (2, 'U')
getCubeWrap (3, 'U') = (1, 'U')
getCubeWrap (3, 'L') = (5, 'D')
getCubeWrap (4, 'D') = (6, 'L')
getCubeWrap (4, 'R') = (2, 'L')
getCubeWrap (4, 'U') = (3, 'U')
getCubeWrap (4, 'L') = (5, 'L')
getCubeWrap (5, 'D') = (6, 'D')
getCubeWrap (5, 'R') = (4, 'R')
getCubeWrap (5, 'U') = (3, 'R')
getCubeWrap (5, 'L') = (1, 'R')
getCubeWrap (6, 'D') = (2, 'D')
getCubeWrap (6, 'R') = (4, 'U')
getCubeWrap (6, 'U') = (5, 'U')
getCubeWrap (6, 'L') = (1, 'D')

-- test data
getCubeWrap_ :: (Int, Char) -> (Int, Char)
getCubeWrap_ (1, 'D') = (4, 'D')
getCubeWrap_ (4, 'D') = (5, 'D')
getCubeWrap_ (5, 'R') = (6, 'R')
getCubeWrap_ (4, 'L') = (3, 'L')
getCubeWrap_ (3, 'L') = (2, 'L')
getCubeWrap_ (2, 'R') = (3, 'R')
getCubeWrap_ (2, 'D') = (5, 'U')
getCubeWrap_ (2, 'U') = (1, 'D')
getCubeWrap_ (2, 'L') = (6, 'U')
getCubeWrap_ (4, 'R') = (6, 'D')
getCubeWrap_ (5, 'D') = (2, 'U')
getCubeWrap_ (3, 'U') = (1, 'R')
getCubeWrap_ x = error $ show x

main = do
    content <- readFile "22.txt"
    let (m, path, x) = loadInput content
    let startingState = ((x, 0), 3)
    let instructions = loadInstructions ('R':path)
    putStrLn $ show $ evalState $ foldl (move m) startingState instructions
    -- > 129015
    -- > 137183
    -- > 107206
