import Data.Maybe (fromJust)
import Data.List (findIndex)

type N = (Int, Int)

mixNum :: [N] -> N -> [N]
mixNum arr x@(i,xVal) = (take x_ ba) ++ [x] ++ (drop x_ ba)
    where index = fromJust $ findIndex ((==i) . fst) arr
          m = (length arr) - 1
          ba = take m $ drop (index + 1) $ cycle arr
          x_ = xVal `mod` m

mixing :: [N] -> [N] -> [N]
mixing key nums = foldl mixNum nums key

coords :: [N] -> [Int]
coords is = map (snd . (c !!)) [1000, 2000, 3000]
    where c = dropWhile ((/=0) . snd) $ cycle is

main = do
    content <- readFile "20.txt"
    -- (a)
    let file1 = zip [1..] (map read $ lines content)
    putStrLn $ show $ sum $ coords $ mixing file1 file1

    -- (b)
    let file2 = map (\(a,b) -> (a, b*811589153)) file1
    putStrLn $ show $ sum $ coords $ (!! 10) $ iterate (mixing file2) file2