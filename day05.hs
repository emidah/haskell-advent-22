import System.IO
import Data.List.Split (splitOn)
import Data.List.Extra (dropEnd, takeEnd)

main = do    
        contents <- readFile "input_day05.txt"

        let piles = toPiles $ reverse $ take 8 $ lines $ contents
        let instructions = map toMove $ map ((map toInt) . (filter notFromOrTo) . (splitOn " ") . (drop 5)) $ drop 10 $ lines $ contents
        -- part 1 --
        print $ solvePuzzle moveOneByOne piles instructions
        -- part 2 --
        print $ solvePuzzle moveAllAtOnce piles instructions

data Move = Move    {
                    i :: Int,
                    from :: Int,
                    to :: Int 
                    }

toMove :: [Int] -> Move 
toMove x = Move (x !! 0) (x !! 1) ( x !! 2) 

solvePuzzle :: ([String] -> Move -> [String]) -> [String] -> [Move] -> String
solvePuzzle strategy piles ins = map last $ foldl strategy piles $ ins

moveOneByOne :: [String] -> Move -> [String]
moveOneByOne p mv = foldl (moveOne mv) p [1..(i mv)] 

moveOne :: Move -> [String] -> Int -> [String]
moveOne mv p _ = map (moveMap (from mv-1) (to mv-1) p 1) [0..((length p) - 1)]

moveMap :: Int -> Int -> [String] -> Int -> Int -> String
moveMap from to p count i | from == i = dropEnd count (p !! i)
                    | to == i = (p !! i) ++ (takeEnd count (p !! from))
                    | otherwise = (p !! i)

moveAllAtOnce :: [String] -> Move -> [String]
moveAllAtOnce p mv = moveAll mv p

moveAll :: Move -> [String] -> [String]
moveAll mv p = map (moveMap  (from mv -1) (to mv -1) p (i mv)) [0..((length p) - 1)]

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