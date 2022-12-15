import Data.Maybe (catMaybes)
import Data.List (sort, nub, find)

type Coords = (Int, Int)
type Input = (Coords, Coords)
type Interval = (Int, Int)

readNumber :: String -> Int
readNumber x = read $ filter (\x -> x `elem` (['0'..'9'])) x

parseInput :: [String] -> Maybe Input
parseInput (_:_:x1:y1:_:_:_:_:x2:y2:_) = Just $ ((readNumber x1, readNumber y1), (readNumber x2, readNumber y2))
parseInput _ = Nothing
  
parseInputs :: String -> [Input]
parseInputs = catMaybes . (map (parseInput . words)) . lines

intersectWithLine :: Int -> Input -> Interval
intersectWithLine y ((sensorX, sensorY), (beaconX, beaconY)) = (sensorX - dist, sensorX + dist)
    where radius = abs (sensorX - beaconX) + abs (sensorY - beaconY)
          dist = radius - abs (y - sensorY)

validInterval :: Interval -> Bool
validInterval (a, b) = a <= b

makeDisjunctive :: [Interval] -> [Interval]
makeDisjunctive (i1@(a,b):i2@(c,d):rest) = if isDisjunctive then i1:(makeDisjunctive $ i2:rest) else (makeDisjunctive $ union:rest)
    where isDisjunctive = b < c
          union = (min a c, max b d)
makeDisjunctive x = x

intervalLength :: Interval -> Int
intervalLength (a, b) = b - a + 1

findDisjunctiveIntervals :: [Interval] -> [Interval]
findDisjunctiveIntervals intervals = makeDisjunctive $ sort $ filter validInterval intervals

inInterval :: Int -> Interval -> Bool
inInterval x (a, b) = a <= x && x <= b

getBeaconsOnLine :: [Input] -> Int -> [Coords]
getBeaconsOnLine input i = nub $ filter ((==i) . snd) $ map snd input

calcTuningFrequency :: Int -> Int -> Int
calcTuningFrequency x y = 4000000 * x + y

findDisjunctiveIntervalsOnLine :: [Input] -> Int -> [Interval]
findDisjunctiveIntervalsOnLine input line = findDisjunctiveIntervals $ map (intersectWithLine line) input

findMidPointX :: [Interval] -> Maybe Int
findMidPointX ((a, b):(c, d):rest) | b + 1 == c - 1 = Just $ b + 1
findMidPointX _ = Nothing

main = do
    content <- readFile "15.txt"
    let input = parseInputs content

    -- part 1
    let intervals = findDisjunctiveIntervalsOnLine input line
    let beaconsOnLine = getBeaconsOnLine input line
    let beaconsInInterval = length $ filter (\b -> any (inInterval b) intervals) (map fst beaconsOnLine)
    putStrLn $ show $ (sum $ map intervalLength intervals) - beaconsInInterval

    -- part 2
    -- this technically works only if the point is in the middle
    let result = find ((/=1) . length . fst) $ map (\y -> (findDisjunctiveIntervalsOnLine input y, y)) [1..limit]
    let frequency = result >>= (\(intervals,y) -> findMidPointX intervals >>= \x -> return $ calcTuningFrequency x y)
    putStrLn $ show $ frequency

    where line = 2000000
          limit = 4000000
