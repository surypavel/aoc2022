-- Using lists is unoptimal but works well enough
import Data.List (nub)

type Coords = (Int, Int, Int)
type Bounds = (Coords, Coords)

parseInput :: String -> [Coords]
parseInput str = map parseCoords $ lines str

parseCoords :: String -> Coords
parseCoords str = read ("(" ++ str ++ ")")

nearby :: Coords -> [Coords]
nearby (x,y,z) = [(x+1,y,z), (x-1,y,z), (x,y+1,z), (x,y-1,z), (x,y,z+1), (x,y,z-1)]

intersections :: [Coords] -> Int
intersections coords = sum $ map (\c -> length $ filter (`elem` coords) (nearby c)) coords

rectangleBounds :: [Coords] -> Bounds
rectangleBounds coords = ((minimum as - 1, minimum bs - 1, minimum cs - 1), (maximum as + 1, maximum bs + 1, maximum cs + 1))
    where as = map (\(a,b,c) -> a) coords
          bs = map (\(a,b,c) -> b) coords
          cs = map (\(a,b,c) -> c) coords

isInBounds :: Bounds -> Coords -> Bool
isInBounds ((x1,y1,z1), (x2,y2,z2)) (a,b,c) = x1 <= a && a <= x2 && y1 <= b && b <= y2 && z1 <= c && c <= z2

bucketFill :: Bounds -> [Coords] -> [Coords] -> [Coords] -> Int
bucketFill _ _ [] coords = 0
bucketFill bounds coords fillFrom filled = score + nextFill
    where newFillFrom = filter (`notElem` coords) $ filter (`notElem` filled) $ nub $ filter (isInBounds bounds) $ concat $ map nearby fillFrom
          nextFill = bucketFill bounds coords newFillFrom (filled ++ fillFrom)
          score = length $ filter (`elem` coords) $ concat $ map nearby fillFrom

main = do
    -- (a)
    content <- readFile "18.txt"
    let coords = parseInput content
    putStrLn $ show $ (length coords * 6) - (intersections coords)

    -- (b) - use "mspaint bucket tool" algo
    let bounds = rectangleBounds coords
    putStrLn $ show $ bucketFill bounds coords [fst bounds] []