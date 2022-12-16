import Data.Maybe (catMaybes, fromJust)
import Data.Char (ord)
import Data.List (sortBy, permutations, nub, inits, sortOn, delete)
import Data.Ord (comparing)
import qualified Data.Map as Map

type Input = (Valve, (Int, [Valve]))
type InputMap = Map.Map Valve ([Valve])
type PathCache = Map.Map (Valve, Valve) Int
type FlowMap = Map.Map Valve (Int)
type Valve = String;

readNumber :: String -> Int
readNumber x = read $ filter (\x -> x `elem` (['0'..'9'])) x

readValve :: String -> Valve
readValve x = filter (\x -> x `elem` (['A'..'Z'])) x

parseInput :: [String] -> Maybe Input
parseInput (_:x:_:_:r:_:_:_:_:rest) = Just $ (readValve x, (readNumber r, map readValve rest))
parseInput _ = Nothing
  
parseInputs :: String -> (InputMap, FlowMap)
parseInputs inputs = (inputMap, flowMap)
    where parsedInputs = catMaybes $ (map (parseInput . words)) $ lines inputs
          flowMap = Map.fromList (map (\(a, b) -> (a, fst b)) parsedInputs)
          inputMap = Map.fromList (map (\(a, b) -> (a, snd b)) parsedInputs)

usefulValves :: FlowMap -> [Valve]
usefulValves flowMap = Map.keys $ Map.filter (>0) flowMap

bubbleDistance :: InputMap -> [Valve] -> [Valve]
bubbleDistance inputMap froms = nub $ concat $ map (\f -> fromJust $ Map.lookup f inputMap) froms

findPath :: InputMap -> Valve -> Valve -> Int
findPath inputMap from to = length $ take 10 $ takeWhile (to `notElem`) $ iterate (bubbleDistance inputMap) [from]

findFlow :: FlowMap -> Valve -> Int
findFlow m x = fromJust $ Map.lookup x m

findCache :: PathCache -> (Valve, Valve) -> Int
findCache m x = fromJust $ Map.lookup x m

evaluate :: PathCache -> FlowMap -> Int -> Valve -> [Valve] -> Int
evaluate _ f l start [] = l * findFlow f start
evaluate _ f l start _ | l <= 0 = 0
evaluate pathCache f l start valves = maximum $ map (\v -> evaluateHead+(evaluateTail v)) valves
    where evaluateHead = l * (findFlow f start)
          evaluateTail = \v -> evaluate pathCache f (l - 1 - (findCache pathCache (start, v))) v (delete v valves)

main = do
    content <- readFile "16.txt"
    let (inputMap, flowMap) = parseInputs content
    let valves = initialValve:(usefulValves flowMap);
    let pathCache = Map.fromList [((x, y), findPath inputMap x y) | x <- valves, y <- valves]
    let mostPressure = evaluate pathCache flowMap 30 initialValve (usefulValves flowMap)
    putStrLn $ show mostPressure
    where initialValve = "AA"