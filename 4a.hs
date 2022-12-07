replaceWithWhitespace :: Char -> String -> String
replaceWithWhitespace _ "" = ""
replaceWithWhitespace d (a:xs) | d == a = ' ' : replaceWithWhitespace d xs
replaceWithWhitespace d (a:xs) = a : replaceWithWhitespace d xs

split :: Char -> String -> [String]
split a b = words $ replaceWithWhitespace a b

parseRow :: String -> [[Int]]
parseRow x = map (map read . split '-') $ split ',' x

verifyRow :: [[Int]] -> Bool
verifyRow ((a:b:_):(c:d:_):_) = (a <= c && b >= d) || (a >= c && b <= d)
verifyRow _ = False

countRows :: [String] -> Int
countRows rows = sum $ map (fromEnum . verifyRow . parseRow) rows

main :: IO ()
main = do
    content <- readFile "4.txt"
    putStrLn $ show $ countRows $ lines content
