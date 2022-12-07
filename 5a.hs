import Data.List (partition)

type Records = [(Char, Int)]
type Instruction = (Int, Int, Int)

findLettersAtIndex :: Int -> String -> Records
findLettersAtIndex n ('[': y :']':' ':xs) = (y, n) : findLettersAtIndex (n+1) xs
findLettersAtIndex n (' ':' ':' ':' ':xs) = findLettersAtIndex (n+1) xs
findLettersAtIndex _ _ = []

findLetters = findLettersAtIndex 1 . (++" ")

flat :: [[a]] -> [a]
flat a = foldl (++) [] a

readNumbers :: [String] -> Maybe Instruction
readNumbers (m:x:_:y:_:z:_) | m == "move" = Just (read x, read y, read z)
readNumbers _ = Nothing

findInstruction :: String -> Maybe Instruction
findInstruction s = readNumbers $ words s

applyInstruction :: Records -> Instruction -> Records
applyInstruction x (n, b, c) = part1b ++ part2 ++ part3
    where p = partition ((==b) . snd) x
          -- (b)
          part1b = map (\u -> (fst u, c)) (take n $ fst p)
          -- (a)
          part1a = reverse part1b
          part2 = drop n $ fst p
          part3 = snd p

applyInstructions :: [String] -> Records
applyInstructions l = foldl applyInstruction letters instructions
    where instructions = [x | Just x <- map findInstruction l]
          letters = flat $ map findLetters l

result :: Int -> Records -> String
result n ((c,m):xs) | m == n = [c]
result n ((_,m):xs) = result n xs
result n [] = ""

maximumRow :: Records -> Int
maximumRow x = foldl max 0 (map snd x)

readResult :: Records -> String
readResult y = flat $ map ((flip result) y) [1..maximumRow y]

main :: IO ()
main = do
    content <- readFile "5.txt"
    putStrLn $ readResult $ applyInstructions $ lines content
