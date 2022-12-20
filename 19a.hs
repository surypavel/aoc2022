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

isMeaningfulUpgrade :: [Resources] -> Resources -> Int -> Bool
isMeaningfulUpgrade costs _ 3 = True
isMeaningfulUpgrade costs x i = not $ all ((<=factories) . (!! i)) costs
    where factories = x !! i

-- heuristics
-- do not attempt to upgrade something i refused to upgrade earlier
-- buy geodes when you can
-- do not upgrade if you are generating enough to cover any cost

nextStates :: [Resources] -> State -> [State]
nextStates b (t, fac, res, sk) = if canBuyGeode then [buyGeode] else waitState:purchaseStates
    where waitState = (t+1, fac, zipWith (+) res fac, unusedOptions)
          resources = zip b [0,1..]
          unusedOptions = map snd $ filter ((==True) . fst) $ zip (map affordable $ map spend resources) [0..]

          notSkipped = \(_,i) -> i `notElem` sk
          isUpgrade = \(_,i) -> isMeaningfulUpgrade b fac i
          spend = \(cost,i) -> (t+1, zipWith (+) fac (unit i), zipWith (-) res (cost), sk)
          stonks = \(x,y,z,_) -> (x,y,zipWith (+) z fac, [])
          affordable = \(_,_,cash,_) -> all (>=0) cash

          canBuyGeode = affordable $ spend (resources !! 3)
          buyGeode = stonks $ spend (resources !! 3)

          options = map spend $ filter isUpgrade $ filter notSkipped resources
          purchaseStates = map stonks $ filter affordable options

evaluate :: Int -> [Resources] -> State -> Int
evaluate lim r (t,_,(_:_:_:g:_),_) | t == lim = g
evaluate lim r (t,_,_,_) | t > lim = 0
evaluate lim r s@(t,(_:_:o:g:_),_,_) = foldl max 0 $ map (evaluate lim r) $ nextStates r s

main = do
    content <- readFile "19.txt"
    let blueprints = unsafeParse content

    -- (a)
    -- [0,0,6,4,30,6,14,40,63,20,33,12,0,112,30,0,187,90,152,140,21,110,92,192,50,156,324,252,145,60]
    -- let score = map (\bp -> fst bp * evaluate 24 (snd bp) (0, [1,0,0,0], [0,0,0,0], [])) blueprints
    -- putStrLn $ show score
    -- putStrLn $ show $ sum score

    -- (b)
    let score = map (\bp -> evaluate 32 (snd bp) (0, [1,0,0,0], [0,0,0,0], [])) $ take 3 blueprints
    putStrLn $ show score
    putStrLn $ show $ product score
