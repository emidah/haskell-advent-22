import System.IO
import Control.Monad
import Data.List.Split
import Data.Sort
import Data.Char

main = do  
        -- part 1
        contents <- readFile "input_day02.txt"
        let arr = map breakSpace $ lines $ contents
        print $ sum $ map (score . toSameOrd) $ arr
        -- part 2
        print $ sum $ map (score . getMoveArr . toSameOrd) $ arr

breakSpace :: String -> [String]
breakSpace x = splitOn " " x

toSameOrd x = [ord1(x !! 0), ord2(x !! 1)] 

score :: [Int] -> Int
score x | (x !! 0) == defeater (x !! 1) = (x !! 1) + 0
        | (x !! 1) == defeater (x !! 0) = (x !! 1) + 6
        | otherwise = (x !! 1) + 3

defeater :: Int -> Int
defeater x  | or [x == 1, x==2] = x+1
            | otherwise = 1

loser :: Int -> Int
loser x  | or [x == 2, x==3] = x-1
            | otherwise = 3


ord1 :: [Char] -> Int
ord1 x = (-) (ord $ (x !! 0)) 64

ord2 :: [Char] -> Int
ord2 x = (-) (ord $ (x !! 0)) 87

getMoveArr :: [Int] -> [Int]
getMoveArr x = [(x !! 0), getMove (x !! 0) (x !! 1)]

getMove :: Int -> Int -> Int
getMove y x | x == 1 = loser y
            | x == 2 = y
            | x == 3 = defeater y
