import Data.List 
---------------------------------------------------------------------------------
-- Exercise 1: Wholemeal programming
---------------------------------------------------------------------------------
fun1' :: [Integer] -> Integer
fun1' = product . map (\x -> x-2) . filter (even)

fun2' :: Integer -> Integer
fun2' = sum . gatherNums

gatherNums :: Integer -> [Integer]
gatherNums 1    = [0]
gatherNums n
    | even n    = [n] ++ gatherNums (n `div` 2 )
    | otherwise = gatherNums (3*n + 1)

---------------------------------------------------------------------------------
-- Exercise 2: Folding with trees 
---------------------------------------------------------------------------------
data Tree a = Leaf | Node Integer (Tree a) a (Tree a) deriving (Show, Eq)

-- the main function to fold a list into a tree 
foldTree :: [a] -> Tree a
foldTree = foldr insertInTree Leaf  

-- height function for inserting into a balanced binary tree, not ordered 
height :: Tree a -> Integer
height Leaf                  = 0
height (Node _ Leaf _ Leaf)  = 0
height (Node _ left _ right) = 1 + max (height left) (height right)

insertInTree :: a -> Tree a -> Tree a 
insertInTree x Leaf                  = Node 0 Leaf x Leaf
insertInTree x (Node n left y right) = case compare (height left) (height right) of 
    GT -> Node (n-1) left y (insertInTree x right)
    EQ -> Node (h+1) left y (insertInTree x right)
        where
            h = height right 
    LT -> Node (n-1) (insertInTree x left) y right

-- Builds a balanced binary tree from a sorted list 
buildFromSorted :: [a] -> Tree a 
buildFromSorted []   = Leaf
buildFromSorted list = Node (height) left (list!!middle) right 
    where 
        middle = (length list) `div` 2
        left   = buildFromSorted $ take middle list 
        right  = buildFromSorted $ drop (middle + 1) list 
        height = floor $ logBase 2 (fromIntegral $ length list)

---------------------------------------------------------------------------------
-- Exercise 3: More folds
---------------------------------------------------------------------------------
-- The xor function below returns True if and only if there is an odd number of True
-- values in the list supplied 
xor :: [Bool] -> Bool
xor [] = False
xor (x:xs) = foldr (funcXOR) x xs 

funcXOR :: Bool -> Bool -> Bool
funcXOR a b = (a && (not b)) || ((not a) && b)

-- The function below is equivalent to map, but using foldr instead 
map' :: (a -> b) -> [a] -> [b]
map' f [] = []
map' f xs = foldr (\x xs -> (f x):xs) [] xs

---------------------------------------------------------------------------------
-- Exercise 4: Finding primes
---------------------------------------------------------------------------------
-- Given integer n, should output list of all ODD primes up to 2n + 2
-- Uses list of numbers to remove, checks this list against numbers of form 2n + 1 
sieveSundaram :: Integer -> [Integer] 
sieveSundaram n = let del = wasteNums n in 
    dropWhile even (2 : [2*x + 1 | x<-[1..n], not (x `elem` del)])

-- Function appropriately calculates set of 'waste' numbers, i.e. those to remove 
wasteNums :: Integer -> [Integer]
wasteNums n = removeDups (filter (<=n) $ map wasteForm (cartProd [1..n] [1..n]))

-- Function calculates the value of a 'waste' number 
wasteForm :: (Integer,Integer) -> Integer
wasteForm (x,y) = x + y + 2*x*y 

-- Removes any duplicates occuring in wasteNums, since sets can't have duplicates
removeDups :: [Integer] -> [Integer]
removeDups = foldl (\seen x -> if x `elem` seen then seen else seen ++ [x]) []

-- Used to group two lists of numbers into pairs, helps a lot with abstraction 
cartProd :: [Integer] -> [Integer] -> [(Integer, Integer)]
cartProd xs ys = [(x,y) | x <- xs, y <- ys] 



