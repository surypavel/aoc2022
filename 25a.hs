parseSnafu :: Char -> Int
parseSnafu '=' = -2
parseSnafu '-' = -1
parseSnafu '0' = 0
parseSnafu '1' = 1
parseSnafu '2' = 2

parseDec :: Int -> Char
parseDec (-2) = '='
parseDec (-1) = '-'
parseDec 0 = '0'
parseDec 1 = '1'
parseDec 2 = '2'

snafuToDec :: [Int] -> Int
snafuToDec x = sum $ map (\(k,v) -> (5 ^ k) * v) $ zip [0..] (reverse x)

decToSnafu :: Int -> [Int]
decToSnafu 0 = []
decToSnafu x = (decToSnafu snafuDiv) ++ [snafuMod]
    where snafuDiv = ((x + 2) `div` 5)
          snafuMod = ((x + 2) `mod` 5) - 2

main = do
    content <- readFile "25.txt"
    let snafuNumbers = map parseDec $ decToSnafu $ sum $ map snafuToDec $ map (map parseSnafu) $ lines content
    putStrLn $ show $ snafuNumbers
