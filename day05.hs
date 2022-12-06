import System.IO
import Data.List.Split


main = do    
        contents <- readFile "input_day05.txt"

        let piles = toPiles $ reverse $ take 8 $ lines $ contents
        let instructions =  map ((map toInt) . (filter notFromOrTo) . (splitOn " ") . (drop 5)) $ drop 10 $ lines $ contents
        print $ instructions
        -- print $ toPiles arr 

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