import Data.Maybe (fromJust, isJust, fromMaybe)
import Data.List (nub)
import qualified Data.Map as Map

type Coords = (Int, Int)
type HurricaneMap = Map.Map Coords [Char]
type HurricaneAvailability = Map.Map Coords [Int]
type Size = Coords

loadInput :: String -> (HurricaneMap, Size)
loadInput s = (Map.fromList a, (w,h))
    -- drop first and last line
    where l = tail $ init $ lines s
          a = concatMap (\(i,line) -> map (\(j,c) -> ((j,i), if c == '.' then [] else [c])) $ zip [0..] $ filter (/='#') line) $ zip [0..] l
          w = (length $ head l) - 2
          h = length l

humanNormalNearby :: Coords -> [Coords]
humanNormalNearby (x,y) = [(x,y), (x+1,y), (x-1,y), (x,y+1), (x,y-1)] -- includes option to stay

hurricaneQuantumNearby :: Coords -> Coords -> [(Coords, Char)]
hurricaneQuantumNearby (w,h) (x,y) = [(((x+1) `mod` (w),y),'<'), (((x-1) `mod` (w),y),'>'), ((x,(y+1) `mod` (h)),'^'), ((x,(y-1) `mod` (h)),'v')]

hurricaneStep :: (HurricaneMap, Size) -> (HurricaneMap, Size)
hurricaneStep (m,s) = (Map.mapWithKey f m, s)
    where f = \k _ -> map snd $ filter (\(c,v) -> v `elem` (fromMaybe [] $ Map.lookup c m)) $ hurricaneQuantumNearby s k

aggregateMaps :: HurricaneAvailability -> (Int, (HurricaneMap, Size)) -> HurricaneAvailability
aggregateMaps a (t, (m, s)) = Map.unionWith (++) a (Map.map (\x -> if x == "" then [t] else []) m)

getHurricaneAvailability :: (HurricaneMap, Size) -> HurricaneAvailability
getHurricaneAvailability m@(_,s) = foldl aggregateMaps Map.empty $ zip [0..] $ take (uncurry lcm s) $ iterate hurricaneStep m

renderMap :: (HurricaneAvailability, Size) -> String
renderMap (r, (w,h)) = (concat $ map (\y -> (concatMap (\x -> show $ fromJust $ Map.lookup (x,y) r) [0..(w-1)]) ++ "\n") [0..(h-1)]) ++ "\n"

findDestination :: Size -> Coords
findDestination (w,h) = (w - 1, h)

bubbleMove :: (HurricaneAvailability, Size) -> (Int, [Coords]) -> (Int, [Coords])
bubbleMove (available,s) (t,c) = (t+1, availableCoords)
    where availableCoords = filter feasible $ nub $ concatMap humanNormalNearby c
          feasible = \coord -> coord == findDestination s || coord == (0,-1) || ((t+1) `mod` (uncurry lcm s)) `elem` (fromMaybe [] $ Map.lookup coord available)

goToDestination :: (HurricaneAvailability, Size) -> (Int, [Coords]) -> Coords -> (Int, [Coords])
goToDestination (availability, s) start destination = (fst $ head $ dropWhile ((notElem destination) . snd) $ iterate (bubbleMove (availability,s)) start, [destination])

main = do
    content <- readFile "24.txt"
    let m@(_,s) = loadInput content
    let availability = getHurricaneAvailability m
    let destination = findDestination s
    -- part 1
    let part1 = foldl (goToDestination (availability,s)) (0, [(0,-1)]) [destination]
    putStrLn $ show $ fst $ part1
    -- part 2
    let part2 = foldl (goToDestination (availability,s)) (0, [(0,-1)]) [destination, (0,-1), destination]
    putStrLn $ show $ fst part2