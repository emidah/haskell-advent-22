import System.IO
import Data.List.Split (splitOn)
import Data.List.Extra (dropEnd, takeEnd)

main = do    
        contents <- readFile "input_day06.txt"
        -- part 1
        print $ findMarker contents 4
        -- part 2
        print $ findMarker contents 14


findMarker :: String -> Int -> Int
findMarker x markerLen = snd $ foldl (saveMarker markerLen) ("", 0) x

saveMarker :: Int -> (String, Int) -> Char -> (String, Int)
saveMarker markerLen (s, i) c     | length s >= markerLen = (s, i)
                        | elem c s = ((fst $ saveMarker markerLen (drop 1 s, i) c), i + 1)
                        | otherwise = (s ++ [c], i + 1)