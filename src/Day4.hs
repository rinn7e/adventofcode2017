
import System.IO

main:: IO ()
main = do  
    handle <- openFile "advent4.txt" ReadMode  
    contents <- hGetContents handle  
    -- print contents  
    print $ countValid $ fmap checkValid $ convert contents  
    -- print $ checkValid ["test3", "test3", "test"]
    hClose handle 


convert :: String -> [[String]]
convert input = 
    fmap words $ lines input

-- checkSentence :: String -> Int
-- checkSentence sentence = 
--     countDuplicate $ word setence

checkValid :: [String] -> Bool
checkValid words =
    checkValid' words []
    
checkValid' :: [String] -> [String] -> Bool
checkValid' words uniqueWords =
    case words of
        x:xs ->
            if x `elem` uniqueWords then
                False
            else 
                checkValid' xs (x:uniqueWords)
        _ -> 
            True

countValid :: [Bool] -> Int
countValid boolList = 
    countValid' 0 boolList

countValid' :: Int -> [Bool] -> Int
countValid' sum boolList = 
    case boolList of
        x:xs ->
            if x == True then
                countValid' (sum + 1) xs
            else 
                countValid' sum xs
        _ ->
            sum

