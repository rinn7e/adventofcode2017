
import Data.Function
import Data.List
import Data.Maybe (fromMaybe)
import Data.List.Split (splitOn)
import System.IO

main:: IO ()
main = do  
    handle <- openFile "advent2.txt" ReadMode  
    contents <- hGetContents handle  
    -- print contents  
    -- print $ result [[5, 1, 9, 5], [7, 5, 3], [2, 4, 6]]
    print $ result $ convert contents
    hClose handle  

convert :: String -> [[Int]]
convert = 
    fmap (\list -> fmap (fromMaybe 0 . readMaybe) list) . fmap (splitOn "\t") . splitOn "\n"


result :: [[Int]] -> Int
result input = 
    input
    & fmap checkSum
    & sum


checkSum :: [Int] -> Int
checkSum l =
    let
        sortList = sort l
    in
        last sortList - head sortList


readMaybe :: (Read a) => String -> Maybe a
readMaybe s = case reads s of
                [(x, "")] -> Just x
                _ -> Nothing