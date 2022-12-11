import Data.Maybe (fromJust, catMaybes)
import Data.Ord (comparing)
import Data.List (groupBy, sortBy)
import Control.Monad.Writer
import qualified Data.Map as Map
import Data.Function (on)

type MonkeyId = Int
type MonkeyMap = Map.Map MonkeyId [Int]
type MonkeyStats = [(Int, Int)]
type MonkeyDef = Map.Map MonkeyId (Int, Int, Int)
type MonkeyF = Map.Map MonkeyId (Int -> Int)

rounds :: Int
-- use rounds = 20 for part (a)
rounds = 10000

monkeyRound :: MonkeyF -> MonkeyDef -> MonkeyMap -> MonkeyId -> Writer MonkeyStats MonkeyMap
monkeyRound fs ds m i = do
    tell $ [(i, length j)]
    return $ foldl (\cm w -> Map.adjust (++[w]) (t w) cm) cleanM newWorries
    where (t1, t2, t3) = fromJust $ Map.lookup i ds
          t = (\x -> if x `rem` t1 == 0 then t2 else t3)
          f = fromJust $ Map.lookup i fs
          j = fromJust $ Map.lookup i m
          gcd = product $ map (\(a,_,_) -> a) $ Map.elems ds
          -- use `div` 3 for part (a)
          newWorries = map ((`rem` gcd) . f) j
          cleanM = Map.insert i [] m

monkeyRounds :: MonkeyF -> MonkeyDef -> MonkeyMap -> Writer MonkeyStats MonkeyMap
monkeyRounds monkeyF monkeyDef monkeyMap = foldM (\m i -> monkeyRound monkeyF monkeyDef m i) monkeyMap (take numberOfItems $ cycle [0..(monkeyLength-1)])
    where monkeyLength = length $ Map.toList $ monkeyMap
          numberOfItems = monkeyLength * rounds

monkeyBusiness :: MonkeyF -> MonkeyDef -> MonkeyMap -> Int
monkeyBusiness monkeyF monkeyDef monkeyMap = product $ take 2 $ sortBy (flip compare) $ map (foldl (\x (_,y) -> x+y) 0) $ groupBy ((==) `on` fst) $ sortBy (comparing fst) $ snd $ runWriter $ monkeyRounds monkeyF monkeyDef monkeyMap

monkeyDebug monkeyF monkeyDef monkeyMap = runWriter $ monkeyRounds monkeyF monkeyDef monkeyMap

onlyNumeric = filter (\x -> x `elem` (['0'..'9']))

parseMonkeyMap :: [String] -> Maybe [Int]
parseMonkeyMap (x:_:xs) | x == "Starting" = Just $ map (read . onlyNumeric) xs
parseMonkeyMap _ = Nothing

readMonkeyMap x = Map.fromList $ zip [0..] $ catMaybes $ map (parseMonkeyMap . words) $ lines x

parseMonkeyDef :: [String] -> Maybe Int
parseMonkeyDef (_:b:_:_:_:f:_) | b == "true:" = Just $ read f
parseMonkeyDef (_:b:_:_:_:f:_) | b == "false:" = Just $ read f
parseMonkeyDef (_:b:_:d:_) | b == "divisible" = Just $ read d
parseMonkeyDef _ = Nothing

groupMonkeyDef :: [Int] -> [(Int, Int, Int)]
groupMonkeyDef (a:b:c:rest) = (a,b,c):(groupMonkeyDef rest)
groupMonkeyDef _ = []

readMonkeyDef x = Map.fromList $ zip [0..] $ groupMonkeyDef $ catMaybes $ map (parseMonkeyDef . words) $ lines x

readVar f x | f == "old" = x
readVar f x = read f

parseMonkeyF :: [String] -> Maybe (Int -> Int)
parseMonkeyF (a:_:_:_:e:f:_) | a == "Operation:" && e == "*" = Just $ \x -> x * readVar f x 
parseMonkeyF (a:_:_:_:e:f:_) | a == "Operation:" && e == "+" = Just $ \x -> x + readVar f x
parseMonkeyF _ = Nothing

readMonkeyF x = Map.fromList $ zip [0..] $ catMaybes $ map (parseMonkeyF . words) $ lines x


main :: IO ()
main = do
    content <- readFile "11.txt"
    let monkeyMap = readMonkeyMap content
    let monkeyDef = readMonkeyDef content
    let monkeyF = readMonkeyF content
    putStrLn $ show $ monkeyBusiness monkeyF monkeyDef monkeyMap