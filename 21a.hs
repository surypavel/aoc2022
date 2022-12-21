import Data.Maybe (catMaybes, fromJust)
import qualified Data.Map as Map

type Pair = (Double, Double)
type Monkey = String
data Val = Op Char Monkey Monkey | Const Int | Var deriving (Show)
type MonkeyMap = Map.Map Monkey Val

readMonkey :: String -> Monkey
readMonkey x = filter (\x -> x `elem` (['a'..'z'])) x

parseVal :: [String] -> Maybe Val
parseVal (x:y:z:_) = Just $ Op (head y) (readMonkey x) (readMonkey z)
parseVal (x:_) = Just $ Const $ read x
parseVal _ = Nothing

parseLine :: String -> Maybe (Monkey, Val)
parseLine line = (parseVal rest) >>= (\x -> Just (readMonkey monkey,x))
    where (monkey:rest) = words line

loadMap :: String -> MonkeyMap
loadMap content = Map.fromList $ catMaybes $ map parseLine $ lines content

getOp :: Char -> (Int -> Int -> Int)
getOp '+' = ((+))
getOp '-' = ((-))
getOp '*' = ((*))
getOp '/' = div

getOpX :: Char -> (Pair -> Pair -> Pair)
getOpX '+' = (\(a,b) (c,d) -> (a+c, b+d))
getOpX '-' = (\(a,b) (c,d) -> (a-c, b-d))
getOpX '*' = (\(a,b) (c,d) -> (a*c, b * c + a * d))
getOpX '/' = (\(a,b) (c,d) -> if d > 0 then error "Unexpected input" else (a / c, b / c))

evaluateVal :: MonkeyMap -> Val -> Int
evaluateVal map (Const x) = x
evaluateVal map (Op sg x y) = (getOp sg) (f x) (f y)
    where f = \m -> evaluateVal map $ fromJust $ Map.lookup m map

evaluateValX :: MonkeyMap -> Val -> Pair
evaluateValX map (Const x) = (fromIntegral x,0)
evaluateValX map (Var) = (0,fromIntegral 1)
evaluateValX map (Op sg x y) = (getOpX sg) (f x) (f y)
    where f = \m -> evaluateValX map $ fromJust $ Map.lookup m map

-- Solve for x
part2 :: MonkeyMap -> Val -> Int
part2 map (Op _ a b) = round $ (\h -> -h) $ (fst dif) / (snd dif)
    where f = \m -> fromJust $ Map.lookup m map
          x = evaluateValX map (f a)
          y = evaluateValX map (f b)
          dif = getOpX ('-') x y

main = do
    content <- readFile "21.txt"
    -- (a)
    let map = loadMap content
    let root = fromJust $ Map.lookup "root" map
    putStrLn $ show $ evaluateVal map $ root

    -- (b)
    let map2 = Map.insert "humn" Var map
    putStrLn $ show $ part2 map2 root 
