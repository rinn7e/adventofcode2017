
import System.IO

main = do
    handle <- openFile "advent5.txt" ReadMode  
    contents <- hGetContents handle  
    -- print contents  
    print $ countStep $ convert contents  
    -- print $ checkValid ["test3", "test3", "test"]
    hClose handle 


    -- print $ countStep' (0, 0, [0, 3, 0, 1, -3])
    -- print $ getNthElement 5 [5, 3, 0, 1, -3]
    -- print $ countStep [0, 3, 0, 1, -3]

-- countStep :: [Int] -> Int
-- countStep input = 
--     let
--         (_, step, _) = countStep' (0, 0, input)
--     in
--         step
convert :: String -> [Int]
convert input = 
    fmap read $ lines input

countStep :: [Int] -> (Int, Int, [Int])
countStep inputList = countStep' (0, 0, inputList)

countStep' :: (Int, Int, [Int]) -> (Int, Int, [Int]) 
countStep' (index, step, input) = 
    case getNthElement index input of
        Nothing ->
            (index, step, input)
        Just elem ->
            countStep' (index + elem, step + 1, changeNthElement index input)


getNthElement :: Int -> [Int] -> Maybe Int
getNthElement index list =
    case splitAt index list of
        (front, element:back) -> Just element
        _ -> Nothing
    
changeNthElement :: Int -> [Int] -> [Int]
changeNthElement index list
    | index < 0   = list
    | otherwise = case splitAt index list of
                    (front, element:back) -> front ++ (element + 1) : back
                    _ -> list