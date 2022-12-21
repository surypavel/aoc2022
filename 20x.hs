-- misunderstanding of how it should work with duplicates
-- moves all units in one step

import Data.List (findIndices)

combine :: Int -> [Int] -> [Int] -> Int -> [Int]
combine x xs others i | i `elem` xs = x : (combine x xs others (i+1))
combine x xs (a:others) i | i `notElem` xs = a : (combine x xs others (i+1))
combine _ _ [] _ = []

mixNum :: [Int] -> Int -> [Int]
mixNum arr x = combine x indices otherLetters 0
    where indices = findIndices (==x) arr
          x_ = x `mod` ((length arr) - 1)
          otherLetters = take (length arr - length indices) $ drop x_ $ cycle $ filter (/=x) arr

mixing :: [Int] -> [Int] -> [Int]
mixing key nums = foldl mixNum nums key

coords :: [Int] -> [Int]
coords is = map (c !!) [1000, 2000, 3000]
    where c = dropWhile (/=0) $ cycle is

main = do
    content <- readFile "20.txt"

    let test = [0,1,1,8]
    putStrLn $ show $ mixing test test

    -- (a)
    let file1 = map read $ lines content
    putStrLn $ show $ sum $ coords $ mixing file1 file1

    -- (b)
    -- let file2 = map (*811589153) file1
    -- putStrLn $ show $ coords $ (!! 10) $ iterate (mixing file2) file2
