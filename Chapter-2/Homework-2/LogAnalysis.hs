{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where
import Log

{-
Built data LogMessage
-}
parseMessage :: String -> LogMessage
parseMessage str = let wordList = words str in case wordList of 
 ("I":timestamp:message) -> LogMessage Info (read timestamp) (unwords message) 
 ("W":timestamp:message) -> LogMessage Warning (read timestamp) (unwords message)
 ("E":level:timestamp:message) -> LogMessage (Error (read level)) (read timestamp) (unwords message)
 _ -> Unknown (unwords wordList)

parse :: String -> [LogMessage]
parse = map parseMessage . lines

insert :: LogMessage -> MessageTree -> MessageTree
insert lmsg Leaf = Node Leaf lmsg Leaf
insert lmsg1@(LogMessage _ ts1 _) (Node left lmsg2@(LogMessage _ ts2 _) right)
 | ts1 > ts2 = Node left lmsg2 (insert lmsg1 right)
 | otherwise = Node (insert lmsg1 left) lmsg2 right 
insert _ tree = tree

build :: [LogMessage] -> MessageTree
build = foldr insert Leaf

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left lmsg right) = inOrder left ++ [lmsg] ++ inOrder right

getMessage :: [LogMessage] -> [String]
getMessage (LogMessage _ _ msg : msgs) = msg: getMessage msgs
getMessage _ = []

severe :: Int -> LogMessage -> Bool
severe minLv (LogMessage (Error lv) _ _)
 | lv > minLv = True
 | otherwise = False
severe _ _ = False

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = getMessage . inOrder . build . filter (severe 50)
