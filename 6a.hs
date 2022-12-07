allDifferent :: String -> Bool
allDifferent (x:xs) = foldl (&&) True (map (/= x) xs) && (allDifferent xs)
allDifferent _ = True

startingPosition :: Int -> Int -> String -> Int
startingPosition m n x | allDifferent $ take m x = n
startingPosition m n (x:xs) = startingPosition m (n+1) xs

findMarker m = show . startingPosition m m

main :: IO ()
main = do
    content <- readFile "6.txt"
    putStrLn $ findMarker 4 content -- (A)
    putStrLn $ findMarker 14 content -- (B)
