
import Data.List
import Data.Maybe

main = do
    -- print $ createNewList [10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25] level (level + 1)
    -- print $ checkLevel 4 1 [ 2,  3,  4,  5,  6,  7,  8,  9]
    -- print $ divideList [10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25]
    -- print $ divideList 2 [10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25]
    -- print $ divideList 1 [ 2,  3,  4,  5,  6,  7,  8,  9]
    -- print $ findEdge 3 (1,[2,3,4,5,6,7,8,9])
    print $ result 4
    -- putStrLn $ "Step for square 4 are: " ++ show square4

result :: Int -> Int
result num = 
    findEdge num $ checkLevel num 1 [ 2,  3,  4,  5,  6,  7,  8,  9]



createNewList :: [Int] -> Int -> Int -> [Int]
createNewList list start final = 
    let
        newList = fmap (+ (8 * start)) list
    in
        if start <= final then
            newList ++ createNewList list (start + 1) final
        else []

checkLevel :: Int -> Int -> [Int] -> (Int, [Int]) 
checkLevel num start list = 
    if num `elem` list then
        (start, list)
    else 
        checkLevel num (start + 1) newList
        where
            newList = createNewList list start (start + 1)

findEdge :: Int ->(Int, [Int]) -> Int
findEdge num (level, list) = 
    sum 
    $ fmap(\edgeList -> 
        if num `elem` edgeList then    
            (fromMaybe 0 (elemIndex num edgeList)) + 1
        else 
            0

    ) 
    $ divideList level list

divideList :: Int -> [Int] -> [[Int]]
divideList level l = 
    [   [(l!!(startIndex))..(l!!(startIndex + space - 1))],
        [(l!!(startIndex + space))..(l!!(startIndex + space*2 - 1))],
        [(l!!(startIndex + space*2))..(l!!(startIndex + space*3 - 1))],
        (
            [(l!!(startIndex + space*3))..(l!!(startIndex + space*4 - (space - level)))]
            -- []
            ++ if startIndex /= 0 then 
                [(head l)..l!!(startIndex - 1)]
            else []
        )

    ] where 
        space = (length l) `div` 4
        startIndex = level - 1
-- 0.[ 1]
-- 1.[ 2,  3,  4,  5,  6,  7,  8,  9]
-- 2.[10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25]
-- 3.[26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49]
-- -- Find out how to [1, 2, 3, 4] -> [Left, [Down,Left], [Right, Down, Left]]

-- -- input :: [Square]
-- -- input = [Access 1, LeftS 2, TopS 3, RightS 4, RightS 5]

-- 37 36  35  34  33  32 31
-- 38 17  16  15  14  13 30
-- 39 18   5   4   3  12 29
-- 40 19   6   1   2  11 28 53
-- 41 20   7   8   9  10 27 52
-- 42 21  22  23  24  25 26 51
-- 43 44  45  46  47  48 49 50

-- isNumberIn num level list = 
--     if num is not in list then
--         isNumberIn num (level + 1) newList
--     where newList = createNewList list time final
--           createNewList list time = 
--                 if time > final then
--                     list + createNewList (time + 1) final
--                 else []
