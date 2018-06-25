-- Section 1, credit card validation
-- Double value of every second digit from beginning of right 
-- Add digits of doubled and undoubled values to form new number 
-- Calculate result mod 10: if 0, valid 

------------------------------------------------------------------------------
-- Exercise 1: toDigits and toDigitsRev 
------------------------------------------------------------------------------
-- A list with elements reversed, so can use it later for reversing toDigits 
rev :: [Integer] -> [Integer]
rev [] = []  -- base case 
rev (s:l) = rev l ++ [s]  -- step case

toDigits :: Integer -> [Integer]
toDigits s 
    | s <= 0 = []  
    | otherwise = toDigits (s `div` 10) ++ [s `mod` 10]

toDigitsRev :: Integer -> [Integer]
toDigitsRev s = rev (toDigits s)  -- using rev here to raise abstraction

------------------------------------------------------------------------------
-- Exercise 2: doubleEveryOther
------------------------------------------------------------------------------
-- Length of a list 
len :: [Integer] -> Integer
len [] = 0  -- base case 
len (_:l) = 1 + len l  -- step case

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []  -- Base case, empty list unaffected 
doubleEveryOther (s:[]) = [s]  -- Base case, single element lists also unaffected 
doubleEveryOther (t:(s:l)) = rev ([2*t, s] ++ doubleEveryOther (rev l))

------------------------------------------------------------------------------
-- Exercise 3: sumDigits 
------------------------------------------------------------------------------
-- Uses toDigits function created before, with extra sum function 
-- Sum of a list  
total :: [Integer] -> Integer
total [] = 0  -- base case
total (s:l) = s + total l  -- step case

sumDigits :: [Integer] -> Integer
sumDigits [] = 0  -- base case 
sumDigits (s:l) = total (toDigits s) + sumDigits l  -- step case 

------------------------------------------------------------------------------
-- Exercise 4: validate  
------------------------------------------------------------------------------
result :: Integer -> Integer
result n = sumDigits (doubleEveryOther (toDigits n))  -- to raise abstraction

validate :: Integer -> Bool 
validate n = result n `mod` 10 == 0

------------------------------------------------------------------------------
-- Exercise 5: Towers of Hanoi 
------------------------------------------------------------------------------
type Peg = String  -- type synonym, i.e. Peg declared as synonym for String  
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
a = "left"
b = "centre"
c = "right"
hanoi n a b c 
    | n <= 0 = []  -- base case, should handle all non positive inputs 
    | otherwise = hanoi (n-1) a c b ++ [(a,b)] ++ hanoi (n-1) c b a 
    -- step case recursively calls hanoi a step down
    -- note for all n ≥ 1, the move (a,b) is included exactly in the middle 