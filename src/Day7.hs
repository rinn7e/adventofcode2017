import System.IO
import Data.List.Split
import Data.List

main = do
    handle <- openFile "input-day-7.txt" ReadMode  
    contents <- hGetContents handle  
    -- print $ fmap (convertToTuple . words) $ lines contents  
    -- print $ fmap words $ lines contents
    print $ getRoot "rmhcw" $ concat $ fmap (convertToParentChild . convertToTuple. words) $ lines contents
    -- print $ convertToTuple ["iqjhw","(119)","->","kwqnczk,","ffulcj"]
    -- print $ concat $ fmap convertToParentChild [
    --           ("ktlj", [])
    --         , ("xhth", [])
    --         , ("ebii", [])
    --         , ("havc", [])
    --         , ("ktlj", [])
    --         , ("padx", ["pbga", "havc", "qoyq"])
    --         , ("fwft", ["ktlj", "cntj", "xhth"])
    --         , ("tknk", ["gyxo", "ebii", "fwft"])
    --     ]
    -- print $ getRoot $ concat $ fmap convertToParentChild [
    --           ("ktlj", [])
    --         , ("xhth", [])
    --         , ("ebii", [])
    --         , ("havc", [])
    --         , ("ktlj", [])
    --         , ("padx", ["pbga", "havc", "qoyq"])
    --         , ("fwft", ["ktlj", "cntj", "xhth"])
    --         , ("tknk", ["gyxo", "ebii", "fwft"])
    --     ]
    hClose handle 

    -- putStrLn "Day 6"
    -- print $ findRoot [
    --       Node "gyxo" (Just "ugml")
    --     , Node "ebii" (Just "ugml")
    --     , Node "jptl" (Just "ugml")
    --     , Node "ugml" (Just "tknk")
    --     , Node "pbga" (Just "padx")
    --     , Node "havc" (Just "padx")
    --     , Node "qoyq" (Just "padx")
    --     , Node "padx" (Just "tknk")
    --     , Node "ktlj" (Just "fwft")
    --     , Node "cntj" (Just "fwft")
    --     , Node "xhth" (Just "fwft")
    --     , Node "fwft" (Just "tknk")
    --     , Node "tknk" Nothing
    --     ]
-- convertToTuple :: [String] -> (String, [String])
convertToTuple :: [String] -> (String, [String])
convertToTuple list =
    let
        parent = head list
        childList = 
            case splitWhen ((==) "->") list of
                front:back -> fmap removeComma (concat back)
                _ -> []
    in
        (parent, childList)

removeComma :: [Char] -> [Char]
removeComma string =
    delete ',' string
--     let
--         parent = headList
--         childList = case splitWhen (=="->") list of
--             (front, element:back) -> back
--             _ -> []
--     in
--     (parent, childList)

convertToParentChild :: (String, [String]) -> [(String, String)]
convertToParentChild (name, children) = 
    fmap (\childName -> (name, childName)) children 


filterChild :: String -> [(String, String)] -> [(String, String)]
filterChild initial list = 
    filter (\(parent, child) -> child == initial) list  

getRoot :: String -> [(String, String)] -> String
getRoot initial list = 
    getRoot' initial list
    where
        getRoot' childPass list = 
            let
                newList = filter (\(parent, child) -> child == childPass ) list
                (parent, child) = head newList
            in
                case newList of
                    [] -> 
                        childPass
                    _ ->
                        getRoot' parent list 
    -- case filterChild "ktlj" list of
    --     x:xs ->
    --         let
    --             (parent, child) = x
    --         in
    --             filter parent list
    --     _ ->


data Node = 
    Node String (Maybe String) deriving (Show)

-- readNode :: String -> Node
-- readNode input = 
--     let
--         list = words input
--     in 
--         Node (head list) (getParent list)

-- getParentList :: [String] -> Maybe String
-- getParentList 



findRoot :: [Node] -> Node
findRoot list =
    head $ filter (\node ->
        case node of
            Node id maybeParent ->
                maybeParent == Nothing
    ) list

convertInput :: String -> [Node]
convertInput input = 
    []