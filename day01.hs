import System.IO
import Control.Monad
import Data.List.Split
import Data.Sort


main = do  
        -- part 1
        contents <- readFile "input_day01.txt"
        let arr = map sum . nestedMap toInt . breakEmpty . lines $ contents
        print $ findMax arr
        -- part 2
        print $ sum . take 3 $ reverse $ sort arr
toInt :: String -> Int
toInt = read

dropPredicate :: String -> Bool
dropPredicate x = x == ""

breakEmpty :: [String] -> [[String]]
breakEmpty x = splitOn [""] x

nestedMap ::(string -> Int) -> [[string]] -> [[Int]]
nestedMap = map . map

findMax :: [Int] -> Maybe Int
findMax [] = Nothing
findMax (x:xs) = 
  case (findMax xs) of
    Nothing -> Just x
    Just m -> if x > m then Just x else Just m