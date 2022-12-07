import Data.List (isPrefixOf, partition)
import Data.Function (on)

data Line = Cd String | Ls | DirLine String | FileLine Int deriving Show

type Directory = (String, Int);

replaceWithWhitespace :: Char -> String -> String
replaceWithWhitespace _ "" = ""
replaceWithWhitespace d (a:xs) | d == a = ' ' : replaceWithWhitespace d xs
replaceWithWhitespace d (a:xs) = a : replaceWithWhitespace d xs

split :: Char -> String -> [String]
split a b = words $ replaceWithWhitespace a b

join :: Char -> [String] -> String
join a [] = "";
join a (x:xs) = x ++ [a] ++ (join a xs)

back :: String -> String
back = ('/':) . (join '/') . init . (split '/')

parseLine :: [String] -> Maybe Line
parseLine (x:y:z:_) | x == "$" && y == "cd" = Just $ Cd z
parseLine (x:y:_) | x == "$" && y == "ls" = Just Ls
parseLine (x:y:_) | x == "dir" = Just $ DirLine y
parseLine (x:y:_) = Just $ FileLine (read x)
parseLine _ = Nothing

proccess :: [Directory] -> Maybe Line -> [Directory]
proccess xd (Just (Cd x)) | x == "/" = ("/", 0):xd
proccess xd@(d:_) (Just (Cd x)) | x == ".." = fst p ++ snd p
    where p = partition (\e -> (fst e) == (back $ fst d)) xd
proccess xd@(d:_) (Just (Cd x)) = (fst d ++ x ++ "/", 0):xd
proccess (d:xd) (Just (FileLine f)) = (fst d, snd d + f):xd
proccess d _ = d

processDirectory :: [String] -> [Directory]
processDirectory = (foldl proccess []) . (map (parseLine . words))

filterSubtree :: Directory -> [Directory] -> [Directory]
filterSubtree d = filter ((isPrefixOf (fst d)) . fst)

wholeDirectorySize :: Directory -> [Directory] -> Int
wholeDirectorySize e d = foldl (+) 0 (map snd $ filterSubtree e d)

mapWholeSize :: [Directory] -> [Directory]
mapWholeSize d = map (\e -> (fst e, wholeDirectorySize e d) ) d

directorySizes :: [String] -> [Int]
directorySizes x = map snd $ mapWholeSize $ processDirectory x

output1 :: [String] -> Int
output1 x = sum $ filter (<= 100000) $ directorySizes x

output2 :: [String] -> Int
output2 x = minimum $ filter (>= spaceNeeded) sizes
    where sizes = directorySizes x
          spaceTaken = maximum sizes
          spaceRemaining = 70000000 - spaceTaken
          spaceNeeded = 30000000 - spaceRemaining

main :: IO ()
main = do
    content <- readFile "7.txt"
    putStrLn $ show $ output1 $ lines content
    putStrLn $ show $ output2 $ lines content