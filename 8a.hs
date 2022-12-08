import qualified Data.Map as Map
import Data.Maybe (fromJust)

type Coords = (Int, Int);
type TreeHashmap = (Map.Map Coords Int, Coords);

mapTrees :: [String] -> TreeHashmap
mapTrees input = (Map.fromList $ foldl (++) [] $ map (\(i,row) -> map (\(j,char) -> ((i, j), read [char])) (zip [1..] row)) $ zip [1..] input, size)
    where size = (length input, length $ head input)

findDirections :: Coords -> Coords -> [[Coords]]
findDirections (w, h) (a, b) = [left, right, top, bottom]
    where left = map (,b) $ reverse [1..(a-1)]
          right = map (,b) [(a+1)..w]
          top = map (a,) $ reverse [1..(b-1)]
          bottom = map (a,) [(b+1)..h]

unsafeLookup :: Map.Map Coords Int -> Coords -> Int
unsafeLookup m i = fromJust $ Map.lookup i m

allTrees :: Coords -> [Coords]
allTrees coords = [(x,y) | x <- [1..(fst coords)], y <- [1..(snd coords)]] 

isVisible :: TreeHashmap -> Coords -> Bool
isVisible (m,size) from = any visible directions
    where height = unsafeLookup m from
          visible = all (<height) . (map $ unsafeLookup m)
          directions = findDirections size from

takeWhileVisibleFrom :: Int -> [Int] -> [Int]
takeWhileVisibleFrom _ [] = []
takeWhileVisibleFrom h (x:xs) | x < h = x : takeWhileVisibleFrom h xs
takeWhileVisibleFrom h (x:xs) | x >= h = [x]

scenicScore :: TreeHashmap -> Coords -> Int
scenicScore (m,size) from = product $ map visible directions
    where height = unsafeLookup m from
          visible = (length) . (takeWhileVisibleFrom height) . (map $ unsafeLookup m)
          directions = findDirections size from

countAllVisible :: TreeHashmap -> Int
countAllVisible m = length $ filter (isVisible m) (allTrees $ snd m)

findMaxScenicScore :: TreeHashmap -> Int
findMaxScenicScore m = maximum $ map (scenicScore m) (allTrees $ snd m)

main :: IO ()
main = do
    content <- readFile "8.txt"
    let forestMap = mapTrees $ lines content
    putStrLn $ show $ countAllVisible forestMap
    putStrLn $ show $ findMaxScenicScore forestMap
