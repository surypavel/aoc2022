import qualified Data.Map as Map
import Data.List (intersect, nub)
import Data.Maybe (listToMaybe, fromMaybe)
import Control.Monad.Writer

type Coords = (Int, Int)
type ElfMap = [Coords]
type Proposals = [Maybe Coords]

nearby :: Coords -> [Coords]
nearby (a, b) = [(x, y) | x <- [a-1, a, a+1], y <- [b-1, b, b+1], x /= y]

-- north south west east
directions :: Int -> Coords -> [([Coords], Coords)]
directions i (a,b) = take 4 $ drop i $ cycle [north, south, west, east]
    where north = ([(x, y) | x <- [a-1, a, a+1], y <- [b-1]], (a,b-1))
          south = ([(x, y) | x <- [a-1, a, a+1], y <- [b+1]], (a,b+1))
          west  = ([(x, y) | x <- [a-1], y <- [b-1, b, b+1]], (a-1,b))
          east  = ([(x, y) | x <- [a+1], y <- [b-1, b, b+1]], (a+1,b))

filterDuplicates :: Proposals -> Proposals
filterDuplicates [] = []
filterDuplicates (a:xs) = h++(filterDuplicates $ map (\x -> if x == a then Nothing else x) xs)
    where h = if (a `elem` xs) then [Nothing] else [a]

propose :: ElfMap -> Int -> Proposals
propose elfMap direction = filterDuplicates $ map (\elf -> if allFree elf then Nothing else listToMaybe $ map snd $ filter free $ directions direction elf) elfMap
    where free = \(coords, _) -> all (`notElem` elfMap) coords
          allFree = \coords -> all free (directions direction coords)

move :: ElfMap -> Int -> ElfMap
move elfMap direction = zipWith fromMaybe elfMap proposals
    where proposals = propose elfMap direction

numberOfMoves :: ElfMap -> Int -> Int 
numberOfMoves elfMap d = if isDone then 1 else 1 + numberOfMoves newMap ((d + 1) `mod` 4)
    where proposals = propose elfMap d
          newMap = zipWith fromMaybe elfMap proposals
          isDone = all (==Nothing) proposals

loadInput :: String -> ElfMap
loadInput s = concatMap (\(i,line) -> map (\(j,c) -> (j,i)) $ filter ((=='#') . snd) $ zip [1..] line) $ zip [1..] l
    where l = lines s

getArea :: ElfMap -> Int
getArea elfMap = product $ map (f . flip map elfMap) [fst, snd]
    where f = \x -> (maximum x) - (minimum x) + 1

main = do
    content <- readFile "23.txt"
    let m = loadInput content
    putStrLn $ show $ (\m -> getArea m - length m) $ fst $ (!!10) $ iterate (\(m_, i) -> (move m_ i, (i + 1) `mod` 4)) (m,0)
    -- painfully slow - how to do it faster?
    putStrLn $ show $ numberOfMoves m 0