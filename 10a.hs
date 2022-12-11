import Data.Maybe (catMaybes)

data Line = Add Int | Noop deriving Show

parseLine :: [String] -> Maybe Line
parseLine (x:y:_) | x == "addx" = Just $ Add $ read y
parseLine (x:_) | x == "noop" = Just Noop
parseLine _ = Nothing

parseLines :: String -> [Line]
parseLines x = catMaybes $ map (parseLine . words) $ lines x

run :: Int -> Line -> [Int]
run s1 (Add x) = [s1, s1+x]
run s1 (Noop) = [s1]

everyf n [] = []
everyf n as  = head as : everyf n (drop n as)

drawPixel True = '#'
drawPixel False = '.'

drawLines [] = []
drawLines items = take 40 items ++ "\n" ++ drawLines (drop 40 items)
calcSignal items = sum $ zipWith (*) [20,60..220] (everyf 40 $ drop 19 items)
drawDisplay items = drawLines $ map drawPixel $ zipWith (\x y -> abs (x - y) <= 1) (cycle [0..39]) items

main :: IO ()
main = do
    content <- readFile "10.txt"
    let cpu = foldl (\i l -> i ++ run (last i) l) [1] (parseLines content)
    putStrLn $ show $ calcSignal $ cpu
    putStrLn $ drawDisplay $ cpu

-- ###...##..#....###..###..####..##..#..#.
-- #..#.#..#.#....#..#.#..#....#.#..#.#..#.
-- #..#.#....#....#..#.###....#..#..#.#..#.
-- ###..#.##.#....###..#..#..#...####.#..#.
-- #.#..#..#.#....#.#..#..#.#....#..#.#..#.
-- #..#..###.####.#..#.###..####.#..#..##..