import System.IO
import Control.Monad
import Data.List.Split
import Data.List
import Data.Sort
import Data.Char
import Data.Set (Set, fromList, intersection, union, elems)

main = do  
        -- part 1
        contents <- readFile "input_day03.txt"
        let arr = nestedMap fromList $ map splitAtHalf $ lines $ contents
        
        print $ getResult arr
         -- part 2
        let arr2 = nestedMap fromList $ toTriplets $ lines $ contents
        print $ getResult arr2

getResult x = sum $ map (getPriority . head . elems) $ mappedIntersection x

getPriority :: Char -> Int
getPriority x = intPriority $ ord x

intPriority :: Int -> Int
intPriority x   | x >= 97 = x-96
                | otherwise = x-38

splitAtHalf :: [Char] -> [[Char]]
splitAtHalf x = pairToList $ splitAt (div (length x) 2) x

pairToList :: (a, a) -> [a]
pairToList (x,y) = [x,y]

nestedMap ::(String -> Set Char) -> [[String]] -> [[Set Char]]
nestedMap = map . map

intersectMap :: ([Set Char] -> Set Char) -> [[Set Char]] -> [Set Char]
intersectMap = map

mappedIntersection :: [[Set Char]] -> [Set Char]
mappedIntersection = map indexIntersect

indexIntersect :: [Set Char] -> Set Char
indexIntersect x = foldr1 intersection x

toTriplets :: [String] -> [[String]]
toTriplets x = chunksOf 3 x
