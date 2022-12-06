import System.IO
import Data.List.Split


main = do    
        contents <- readFile "input_day04.txt"
        -- create array of [[[a,b],[x,y]]]
        let arr = map (map (map toInt) . map breakDash) $ map breakComma $ lines $ contents
        
        -- part 1 : get which ranges either contain each other
        print $ sum $ map (forPair $ eitherWay contains) arr
        -- part 2 : get which ranges overlap
        print $ sum $ map (forPair $ eitherWay overlap) arr

toInt :: String -> Int
toInt = read

breakDash :: String -> [String]
breakDash x = splitOn "-" x

breakComma :: String -> [String]
breakComma x = splitOn "," x

forPair :: ([Int] -> [Int] -> Bool) -> [[Int]] -> Int
forPair y x | y (x !! 0) (x !! 1) = 1
            | otherwise = 0

eitherWay :: ([a] -> [a] -> Bool) -> [a] -> [a] -> Bool
eitherWay x y z   | x y z = True
                  | x z y = True
                  | otherwise = False

contains :: [Int] -> [Int] -> Bool
contains x y = (x !! 0) >= (y !! 0) && (x !! 1 <= y !! 1)

overlap :: [Int] -> [Int] -> Bool
overlap x y = (x !! 0) <= (y !! 1) && (y !! 0) <= (x !! 1)