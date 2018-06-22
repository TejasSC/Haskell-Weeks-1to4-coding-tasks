{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where
import Log

-- 'I' is for informational messages 
-- 'W' for warnings
-- 'E' for errors 

-- Error messages have scale on 1 to 100 (i.e. mild to extreme)

---------------------------------------------------------------------------------
-- Exercise 1: parseMessage
---------------------------------------------------------------------------------
parseMessage :: String -> LogMessage  -- parsing function 

-- $ means that anything after it will take precedence over anything before it 
parseMessage s = case words s!!0 of
    "I" -> LogMessage Info (read $ words s!!1 :: TimeStamp) (unwords $ drop 2 $ words s)
    "W" -> LogMessage Warning (read $ words s!!1 :: TimeStamp) (unwords $ drop 2 $ words s)
    "E" -> LogMessage (Error (read $ words s!!1 :: TimeStamp)) (read $ words s!!2 :: TimeStamp) 
           (unwords $ drop 3 $ words s) 
    _   -> Unknown s  -- Easiest to deal with 

parse :: String -> [LogMessage]
-- Map function takes function and list as arguments, returns list under image of function 
parse s = map parseMessage $ lines s

---------------------------------------------------------------------------------
-- Exercise 2: insert
---------------------------------------------------------------------------------
-- Raising abstraction with a single function to get the timestamp from a LogMessage first 
getInt :: LogMessage -> TimeStamp
getInt (LogMessage _ a _) = a
getInt (Unknown _) = 0
getInt (LogMessage (Error _) b _ ) = b

insert :: LogMessage -> MessageTree -> MessageTree
insert lmX Leaf = Node Leaf lmX Leaf  -- base case, empty node/Leaf 
insert lmX (Node t lmY s)
    | getInt lmX <= getInt lmY = Node (insert lmX t) lmY s  -- step case, less than or equal
    | getInt lmX > getInt lmY  = Node t lmY (insert lmX s)  -- step case, opposite to above 
insert (Unknown _) (Node t lmY s) = Node t lmY s  -- Unknowns should not affect a messageTree 

---------------------------------------------------------------------------------
-- Exercise 3: build
---------------------------------------------------------------------------------
-- Makes a messageTree from a list of messages 

-- Getting timestamps from a list of log messages 
getStamps :: [LogMessage] -> [TimeStamp]
getStamps [] = []
getStamps (lmX:lmList) = [getInt lmX] ++ getStamps lmList

build :: [LogMessage] -> MessageTree
build [] = Leaf  -- Takes empty list, produces empty node 
-- left subtree has half the list, right subtree has the other half 
build (lmX:lmList) = Node 
                     (build (filter (\msg -> getInt msg < getInt lmX) lmList)) lmX
                     (build (filter (\msg -> getInt msg > getInt lmX) lmList)) 

---------------------------------------------------------------------------------
-- Exercise 4: inOrder
---------------------------------------------------------------------------------
inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []  -- No log messages in an empty node, so empty list generated 
inOrder (Node t lmY s) = inOrder t ++ [lmY] ++ inOrder s  -- Recursively makes list 

---------------------------------------------------------------------------------
-- Exercise 5: whatWentWrong
---------------------------------------------------------------------------------
-- Takes unsorted list of LogMessages, returns list of messages of any errors with 
-- severity of 50 or more 

-- Function to get severity of all kinds of logMessages, default for non-errors is 0
getSevere :: LogMessage -> Int 
getSevere (LogMessage (Error b) _ _) = b
getSevere (LogMessage _ a _) = 0
getSevere (Unknown _) = 0

-- building a sorted list of logMessages, using build and then inOrder 
sortList :: [LogMessage] -> [LogMessage]
sortList lmList = inOrder $ build lmList

-- Getting the actual description of sever error LogMessages 
getString :: [LogMessage] -> String
getString [(LogMessage (Error _) _ s)] = s  -- Order matters in specifying how to get strings
getString [(Unknown _)] = ""  -- Say empty here because we are not interested in non-errors
getString [(LogMessage _ _ _)] = "" 
getString [] = ""

-- Extract messages of type error, of whose severity >= 50
bad :: [LogMessage] -> [LogMessage]
bad lmList = filter (\msg -> getSevere msg >= 50) lmList

-- Putting the descritptions in a new list 
whatWentWrong :: [LogMessage] -> [String]
whatWentWrong [] = []
-- Filter out all non-error messages based on their appropriate strings defined above 
whatWentWrong (lmX:lmList) = filter (/="") (getString (bad [lmX]) : whatWentWrong (bad lmList))


