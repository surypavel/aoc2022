import qualified Data.Map as Map
import Data.Foldable (find)
import Data.List (nub)
import Data.Maybe (fromJust, isJust)
import Data.Char (ord)

type Coords = (Int, Int)
type AreaMap = Map.Map Coords Char
type DistanceMap = Map.Map Coords Int
type Queue = [Coords]

mapChar :: Char -> Char
mapChar 'S' = 'a'
mapChar 'E' = 'z'
mapChar x = x

readMap :: String -> AreaMap
readMap input = Map.fromList $ concat $ map (\(i,row) -> map (\(j,char) -> ((i, j), char)) (zip [1..] row)) $ zip [1..] inputLines
    where inputLines = lines input

findSpots :: AreaMap -> Char -> [Coords]
findSpots m c = map fst $ filter ((== c) . snd) $ Map.assocs m

nearby :: Coords -> [Coords]
nearby (a, b) = [(a+1, b), (a-1, b), (a, b+1), (a, b-1)]

canReach :: Maybe Char -> Maybe Char -> Bool
canReach (Just from) (Just to) = toInt to <= toInt from + 1
    where toInt = ord . mapChar
canReach _ _ = False

notVisited :: DistanceMap -> Coords -> Bool
notVisited dm x = (isJust $ Map.lookupIndex x dm) == False

findDistances :: AreaMap -> (DistanceMap, Queue, Int) -> (DistanceMap, Queue, Int)
findDistances am x@(_, [], _) = x
findDistances am (dm, q, i) = findDistances am (dm2, q2, i+1)
    where dm2 = foldl (\m_ q_ -> Map.insertWith (\x _ -> x) q_ i m_) dm q
          isValid q_ x = notVisited dm2 x && canReach (Map.lookup q_ am) (Map.lookup x am)
          q2 = nub $ concat $ map (\q_ -> filter (isValid q_) $ nearby q_) q

findValueAtSpot :: Coords -> (DistanceMap, Queue, Int) -> Maybe Int
findValueAtSpot c (m, _, _) = Map.lookup c m

main :: IO ()
main = do
    content <- readFile "12.txt"
    let areaMap = readMap content
    let startingSpots = findSpots areaMap 'S'
    let spotsWithElevationA = findSpots areaMap 'a'
    let endingSpot = head $ findSpots areaMap 'E'
    putStrLn $ show $ findValueAtSpot endingSpot $ findDistances areaMap (Map.empty, startingSpots, 0)
    putStrLn $ show $ findValueAtSpot endingSpot $ findDistances areaMap (Map.empty, spotsWithElevationA ++ startingSpots, 0)
