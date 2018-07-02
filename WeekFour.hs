import Data.List 
---------------------------------------------------------------------------------
-- Exercise 1: Wholemeal programming
---------------------------------------------------------------------------------
fun1' :: [Integer] -> Integer
fun1' = product . map (subtract 2) . filter even

fun2' :: Integer -> Integer
fun2' = sum . subFunc

-- Need only evens, greater than 1 
subFunc :: Integer -> [Integer]
subFunc = filter even . takeWhile (>1) . iterate 
          (\a -> if even a then a `div` 2 else (3*a + 1))
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
insertInTree x Leaf                  = Node 0 Leaf x Leaf  -- Base case 
-- 3 other base cases for inserting into a balanced binary tree 
insertInTree x (Node h Leaf y Leaf)  = Node 1 (insertInTree x Leaf) y Leaf
insertInTree x (Node h Leaf y right) = Node h (insertInTree x Leaf) y right
insertInTree x (Node h left y Leaf)  = Node h left y (insertInTree x Leaf)
-- Step case, to ensure no heights of any two subtrees at the bottom differ by > 1
insertInTree x (Node n left y right) = case compare (height left) (height right) of 
    GT -> Node (n-1) left y (insertInTree x right)
    EQ -> Node (h+1) left y (insertInTree x right)
        where
            h = height right 
    LT -> Node (n-1) (insertInTree x left) y right

---------------------------------------------------------------------------------
-- Exercise 3: More folds
---------------------------------------------------------------------------------
-- The xor function below returns True if and only if there is an odd number of True
-- values in the list supplied 
xor :: [Bool] -> Bool
xor = foldr funcXOR False 

-- Returns true if and only if a and b are not the same 
funcXOR :: Bool -> Bool -> Bool
funcXOR a b = (a /= b)

-- The function below is equivalent to map, but using foldr instead 
map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x xs -> (f x):xs) []

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
removeDups = foldl' (\seen x -> if x `elem` seen then seen else seen ++ [x]) []

-- Used to group two lists of numbers into pairs, helps a lot with abstraction 
cartProd :: [Integer] -> [Integer] -> [(Integer, Integer)]
cartProd xs ys = [(x,y) | x <- xs, y <- ys] 

