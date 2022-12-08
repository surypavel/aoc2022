import qualified Data.Map as Map
import Data.Maybe (fromJust)

type Coords = (Int, Int);
type TreeHashmap = (Map.Map Coords Int, Coords);

mapTrees :: [String] -> TreeHashmap
mapTrees input = (Map.fromList $ foldl (++) [] $ map (\(i,row) -> map (\(j,char) -> ((i, j), read [char])) (zip [1..] row)) $ zip [1..] input, (length input, length $ head input))

findDirections :: Coords -> Coords -> [[Coords]]
findDirections (w, h) (a, b) = [left, right, top, bottom]
    where left = map (\i -> (i, b)) $ reverse [1..(a-1)]
          right = map (\i -> (i, b)) [(a+1)..w]
          top = map (\i -> (a, i)) $ reverse [1..(b-1)]
          bottom = map (\i -> (a, i)) [(b+1)..h]

isVisible :: TreeHashmap -> Coords -> Bool
isVisible (m,size) from = foldl (||) False (map visible directions)
    where height = fromJust $ Map.lookup from m
          visible = all (<height) . (map (\i -> fromJust $ Map.lookup i m))
          directions = findDirections size from

takeWhileVisibleFrom :: Int -> [Int] -> [Int]
takeWhileVisibleFrom _ [] = []
takeWhileVisibleFrom h (x:xs) | x < h = x : takeWhileVisibleFrom h xs
takeWhileVisibleFrom h (x:xs) | x >= h = [x]

scenicScore :: TreeHashmap -> Coords -> Int
scenicScore (m,size) from = foldl (*) 1 (map visible directions)
    where height = fromJust $ Map.lookup from m
          visible = (length) . (takeWhileVisibleFrom height) . (map (\i -> fromJust $ Map.lookup i m))
          directions = findDirections size from

allTrees :: Coords -> [Coords]
allTrees coords = [(x,y) | x <- [1..(fst coords)], y <- [1..(snd coords)]] 

countAllVisible :: [String] -> Int
countAllVisible l = length $ filter (isVisible forestMap) (allTrees $ snd forestMap)
    where forestMap = mapTrees l

findMaxScenicScore :: [String] -> Int
findMaxScenicScore l = foldl max 0 $ map (scenicScore forestMap) (allTrees $ snd forestMap)
    where forestMap = mapTrees l

main :: IO ()
main = do
    content <- readFile "8.txt"
    putStrLn $ show $ countAllVisible $ lines content
    putStrLn $ show $ findMaxScenicScore $ lines content
