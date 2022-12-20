import Data.List (sortBy)
import Data.Ord (comparing)

type Resources = [Int]
type Blueprint = (Int, [Resources])
type State = (Int, Resources, Resources, [Int])

readNumber :: String -> Int
readNumber x = read $ filter (\x -> x `elem` (['0'..'9'])) x

unsafeParseLine :: String -> Blueprint
unsafeParseLine s = (id, [[a, 0, 0, 0], [b, 0, 0, 0], [c, d, 0, 0], [e, 0, f, 0]])
    where w = words s
          (id:a:b:c:d:e:f:_) = map readNumber [w !! 1, w !! 6, w !! 12, w !! 18, w !! 21, w !! 27, w !! 30]

unsafeParse :: String -> [Blueprint]
unsafeParse = (map unsafeParseLine) . lines

unit :: Int -> [Int]
unit n = (replicate n 0) ++ 1:(repeat 0)

isAffordable :: State -> Bool
isAffordable (_, _, cash, _) = all (>=0) cash

nextStates :: [Resources] -> State -> [State]
nextStates b (t, fac, res, sk) = if length purchaseStates == 4 then [last purchaseStates] else waitState:purchaseStates
    where waitState = (t+1, fac, zipWith (+) res fac, map snd $ filter ((==True) . fst) $ zip (map isAffordable options) [0..])
          skipped = \(_,i) -> i `notElem` sk
          options = map (\(cost,i) -> (t+1, zipWith (+) fac (unit i), zipWith (-) res (cost), sk)) $ filter skipped $ zip b [0,1..]
          purchaseStates = map (\(x,y,z,_) -> (x,y,zipWith (+) z fac, [])) $ filter isAffordable options

hasObsidian :: State -> Bool
hasObsidian (_, (_:_:x:_), _, _) = x > 0

hasGeode :: State -> Bool
hasGeode (_, (_:_:_:x:_), _, _) = x > 0

hasOre :: Int -> State -> Bool
hasOre n (_, (x:_), _, _) = x >= n

findState :: (State -> Bool) -> [Resources] -> [State] -> [State]
findState f r [] = []
findState f r ss@((t,_,_,_):_) = if (length states == 0) then findState f r (concat $ map (nextStates r) ss) else states
    where states = filter f ss

evaluate :: Int -> [Resources] -> State -> Int
evaluate lim r (t, _, (_:_:_:g:_),_) | t == lim = g
evaluate lim r (t, _, (_:_:_:g:_),_) | t > lim = 0
evaluate lim r (t, (_:_:o:_:_),_ ,_) | t > 15 && o == 0 = 0
evaluate lim r (t, (_:_:_:g:_),_, _) | t > 22 && g == 0 = 0
evaluate lim r s@(t,(_:_:o:g:_),_,_) = foldl max 0 $ map (evaluate lim r) $ nextStates r s

evaluateWithHeuristics :: [Resources] -> Int
evaluateWithHeuristics bp = foldl max 0 $ map (evaluate 24 bp) $ states1
    where states1 = findState hasGeode bp $ [(0, [1,0,0,0], [0,0,0,0], [])]

main = do
    content <- readFile "19.txt"
    let blueprints = unsafeParse content
    let score = map (\bp -> fst bp * evaluate 24 (snd bp) (0, [1,0,0,0], [0,0,0,0], [])) blueprints
    -- let score = map (\bp -> fst bp * evaluate 24 (snd bp) (0, [1,0,0,0], [0,0,0,0], [])) blueprints
    -- takes eternity
    -- [0,0,6,4,30,6,14,40,63,20,33,12,0,112,30,0,187,90,152,140,21,110,92,192,50,156,324,252,145,60]
    putStrLn $ show score
    putStrLn $ show $ sum score
