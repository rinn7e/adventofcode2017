import Data.List
import Data.Maybe

main = do 
    -- print $ findCycle [0, 2, 7, 0]
    print $ spread $ setZero $ findHighest [0, 2, 7, 0]
    print $ setZero $ findHighest [0, 2, 7, 0]
    print $ findCycle [0, 2, 7, 0]
    print $ findCycle [4,1,15,12,0,9,9,5,5,8,7,3,14,5,12,3]
    print $ spread (7,2,[0,2,0,0])
    -- print $ setZero 7 [0, 2, 7, 0]

    
findCycle :: [Int] -> Int
findCycle list = 
    findCycle' 0 list []
    where
        findCycle' :: Int -> [Int] -> [[Int]] -> Int
        findCycle' cycle list seen = 
            case list `elem` seen of
                True -> 
                    cycle
                False ->
                    findCycle' (cycle + 1) (redistribute list) (list:seen)

redistribute :: [Int] -> [Int]        
redistribute list =
    spread $ setZero $ findHighest list

findHighest :: [Int] -> (Int, [Int])
findHighest list = 
    (findHighest' list 0, list)
    where 
        findHighest':: [Int] -> Int -> Int
        findHighest' list high = 
            case list of 
                x:xs -> 
                    if x > high then
                        findHighest' xs x
                    else
                        findHighest' xs high
                _ -> 
                    high



setZero :: (Int, [Int]) -> (Int, Int, [Int])
setZero (high, list) = 
    let
        index = fromMaybe 0 $ high `elemIndex` list
    in
        (high, index, zeroNthElement index list)

spread :: (Int, Int, [Int]) -> [Int]
spread (high, index, list) =
    spread' high (index+1) list
    where
        spread':: Int -> Int -> [Int] -> [Int]
        spread' valueToSpread currentIndex list =
            case valueToSpread of
                0 -> list
                _ -> 
                    case getNthElement currentIndex list of
                        Nothing -> 
                            spread' valueToSpread 0 list 
                        Just elem ->
                            spread' (valueToSpread - 1) (currentIndex + 1) (incrementNth currentIndex list)

                            
                


getNthElement :: Int -> [Int] -> Maybe Int
getNthElement index list =
    case splitAt index list of
        (front, element:back) -> Just element
        _ -> Nothing


zeroNthElement :: Int -> [Int] -> [Int]
zeroNthElement index list =
    if index < 0 then 
        list
    else 
        case splitAt index list of
            (front, element:back) -> front ++ 0 : back
            _ -> list

incrementNth :: Int -> [Int] -> [Int]
incrementNth index list =
    if index < 0 then 
        list
    else 
        case splitAt index list of
            (front, element:back) -> front ++ (element + 1) : back
            _ -> list