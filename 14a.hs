import qualified Data.Map as Map
import Data.Maybe (isNothing, isJust, fromJust)

type Coords = (Int, Int)
type RockMap = Map.Map Coords Bool
type SandMap = Map.Map Coords Bool

generator :: Coords
generator = (500,0)

rangeList :: Int -> Int -> [Int]
rangeList a b | a /= b = [a, a + (signum $ b - a)..b]
rangeList a b = [a]

parsePath :: String -> Coords
parsePath string = read $ "(" ++ string ++ ")"

pathFrom :: Coords -> Coords -> [Coords]
pathFrom (a,b) (c,d) = [(x,y) | x <- rangeList a c, y <- rangeList b d]

parseLine :: [String] -> [Coords]
parseLine (x:_:y:rest) = pathFrom (parsePath x) (parsePath y) ++ (parseLine $ y:rest)
parseLine _ = []

generateRockMap :: String -> RockMap
generateRockMap input = Map.fromList rockList
    where rockList = map (,True) $ concat $ map (parseLine . words) $ lines input

fall :: (Coords -> Bool) -> Coords -> Maybe Coords
fall free (a,b) | b > 500 = Nothing
fall free (a,b) | free (a,b+1) = fall free (a,b+1)
fall free (a,b) | free (a-1,b+1) = fall free (a-1,b+1)
fall free (a,b) | free (a+1,b+1) = fall free (a+1,b+1)
fall free (a,b) | free (a,b) = Just (a,b)
fall free (a,b) = Nothing

dropSand :: RockMap -> SandMap -> Maybe Coords
dropSand r s = fall free generator
    where bottom = findBottom r
          isBottom (_,y) = y == bottom + 2
          freeFromRocks coords = (isNothing $ Map.lookup coords r) && (isNothing $ Map.lookup coords s)
          -- part (a)
          -- free coords = freeFromRocks coords
          -- part (b)
          free coords = freeFromRocks coords && (not $ isBottom coords)

dropSandAndUpdate :: RockMap -> Maybe SandMap -> Maybe SandMap
dropSandAndUpdate r Nothing = Nothing
dropSandAndUpdate r (Just s) = if isNothing coords then Nothing else Just $ Map.insert (fromJust coords) True s
    where coords = dropSand r s

sandstorm rockMap = (\x -> x-1) $ length $ takeWhile isJust $ iterate (dropSandAndUpdate rockMap) (Just Map.empty)

findBottom :: RockMap -> Int
findBottom rockMap = maximum $ map snd $ Map.keys rockMap

main = do
    content <- readFile "14.txt"
    let rockMap = generateRockMap content
    putStrLn $ show $ sandstorm rockMap
