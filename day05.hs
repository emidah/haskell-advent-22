import System.IO
import Data.List.Split (splitOn)
import Data.List.Extra (dropEnd, takeEnd)

main = do    
        contents <- readFile "input_day05.txt"

        let piles = toPiles $ reverse $ take 8 $ lines $ contents
        let instructions = map ((map toInt) . (filter notFromOrTo) . (splitOn " ") . (drop 5)) $ drop 10 $ lines $ contents
        -- part 1 --
        print $ solvePuzzle moveOneByOne piles instructions
        -- part 2 --
        print $ solvePuzzle moveAllAtOnce piles instructions

solvePuzzle strategy piles ins = map last $ filter isNotEmpty $ doMoves strategy piles $ ins

isNotEmpty :: String -> Bool
isNotEmpty s = s /= ""

doMoves :: ([String] -> [Int] -> [String]) -> [String] -> [[Int]] -> [String]
doMoves m p i = foldl m p i

moveOneByOne :: [String] -> [Int] -> [String]
moveOneByOne p i = foldl (moveOne (i !! 1) (i !! 2)) p [1..(i !! 0)] 

moveOne :: Int -> Int -> [String] -> Int -> [String]
moveOne from to p _ = map (moveMap (from-1) (to-1) p 1) [0..((length p) - 1)]

moveMap :: Int -> Int -> [String] -> Int -> Int -> String
moveMap from to p count i | from == i = dropEnd count (p !! i)
                    | to == i = (p !! i) ++ (takeEnd count (p !! from))
                    | otherwise = (p !! i)

moveAllAtOnce :: [String] -> [Int] -> [String]
moveAllAtOnce p i = moveAll (i !! 1) (i !! 2) p (i !! 0)

moveAll :: Int -> Int -> [String] -> Int -> [String]
moveAll from to p count = map (moveMap  (from-1) (to-1) p count) [0..((length p) - 1)]

toPiles :: [[Char]] -> [[Char]]
toPiles x = map (toPile $ filterAll x) [0..8]

filterAll :: [[Char]] -> [[Char]]
filterAll x = map indexFilter x

indexFilter :: [Char] -> [Char]
indexFilter x = filterByIndexList (getIndexList x) x

getIndexList :: [Char] -> [Int]
getIndexList x = [1,5..(length x)]

filterByIndexList :: [Int] -> [Char] -> [Char]
filterByIndexList y x = map fst $ filter (indexPredicate y) $ zip x [0..]

indexPredicate :: [Int] -> (Char, Int) -> Bool
indexPredicate x (y, z) = elem z x

toPile :: [[Char]] -> Int -> [Char]
toPile x y = filter notEmpty $ map (!! y) x

notEmpty :: Char -> Bool
notEmpty x = x /= ' '

toInt :: String -> Int
toInt = read

notFromOrTo :: String -> Bool
notFromOrTo x   | x == "from" = False
                | x == "to" = False
                | otherwise = True