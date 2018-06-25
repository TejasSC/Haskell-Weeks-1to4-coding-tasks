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
-- Pattern matching: use this wherever you can!
-- Also, no need to say read 'as a timeStamp': type inferece in haskell sorts that out already 
parseMessage s = case words s of
    ("I":x:text)   -> LogMessage Info (read x) (unwords text)
    ("W":x:text)   -> LogMessage Warning (read x) (unwords text)
    ("E":x:y:text) -> LogMessage (Error (read x)) (read y) (unwords text) 
    _              -> Unknown s  -- Easiest to deal with 

-- Map function takes function and list as arguments, returns list under image of function 
parse :: String -> [LogMessage]
parse s = map parseMessage $ lines s

---------------------------------------------------------------------------------
-- Exercise 2: insert
---------------------------------------------------------------------------------
-- Raising abstraction with a single function to get the timestamp from a LogMessage first 
getInt :: LogMessage -> TimeStamp
getInt (LogMessage _ a _) = a
getInt (Unknown _) = 0

insert :: LogMessage -> MessageTree -> MessageTree
-- Unknown should be the first case in the function; otherwise tree unintentionally affected 
insert (Unknown _) (Node t lmY s) = Node t lmY s  -- Unknowns should not affect a messageTree 
insert lmX Leaf = Node Leaf lmX Leaf  -- base case, empty node/Leaf 
insert lmX (Node t lmY s) = case compare (getInt lmX) (getInt lmY) of 
    GT        -> Node t lmY (insert lmX s)
    otherwise -> Node (insert lmX t) lmY s

---------------------------------------------------------------------------------
-- Exercise 3: build
---------------------------------------------------------------------------------
-- Makes a messageTree from a list of messages 
build :: [LogMessage] -> MessageTree
build [] = Leaf  -- Takes empty list, produces empty node 
build (lmX:lmList) = insert lmX (build lmList)  -- Tree structure as recursive call to build 

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
getString :: LogMessage -> String
getString (LogMessage (Error _) _ s) = s  -- Order matters in specifying how to get strings
getString (Unknown _) = ""  -- Say empty here because we are not interested in non-errors
getString (LogMessage _ _ _) = "" 

-- Putting the descritptions in a new list 
whatWentWrong :: [LogMessage] -> [String]
-- Filter out all non-error messages based on their appropriate strings defined above 
whatWentWrong messages = map getString $ filter (\msg -> getSevere msg >= 50) messages
