module Golf where 
import Data.List 
import Control.Monad 
---------------------------------------------------------------------------------
-- Exercise 1: hopscotch 
---------------------------------------------------------------------------------
-- nth output list has every nth element from the input list 
skips :: [a] -> [[a]]
skips [] = []
skips xs = [everyNth n xs | n <- [1..length xs]]
-- Above technique of range assignment equivalent to increasing a value 

everyNth :: Int -> [a] -> [a]
everyNth n xs = case drop (n-1) xs of 
                (y:ys) -> y : everyNth n ys 
                []     -> []

---------------------------------------------------------------------------------
-- Exercise 2: local Maxima 
---------------------------------------------------------------------------------

-- head originally a partial function, now we can make it total, and safe 
safeHead :: [Int] -> Int
safeHead [] = 0 
safeHead (x:_) = x

-- Finds local maxima in a list of Ints, 
localMaxima :: [Int] -> [Int]
localMaxima [] = []
localMaxima [a] = []
localMaxima [a,b] = []
localMaxima (a:b:xs)
  | b > a && b > (safeHead xs) = b : localMaxima xs 
  | otherwise  = localMaxima (b:xs) 

---------------------------------------------------------------------------------
-- Exercise 3: histogram 
---------------------------------------------------------------------------------
-- Takes as input a list of Ints between 0 and 9
-- Outputs vertical histogram showing how many of each number was in the input list 

-- To save time, I simply put the function from [Int] to IO (), rather than having to 
-- do putStr(histogram) everytime I test it 

histogram :: [Int] -> IO () 
histogram intData = mapM_ putStrLn $ reverse $ takeWhile (any (/= ' ')) $ transpose
  $ map (\n -> show n ++ "=" ++ replicate (length (filter (== n) intData)) '*' ++ repeat ' ') 
  $ [0..9]

-- All the stuff below are attempts I tried before I found the working solution 
  {--
type Point = (Int, Int)
getFreq :: Point -> Int
getFreq (_,n) = n

numFreqPoint :: [Int] -> Point
numFreqPoint list = (safeHead list, length list)

sortData :: [Int] -> [[Int]]
sortData someList = group $ sort someList

obtainPoints :: [Int] -> [Point] 
obtainPoints someList = map numFreqPoint (sortData someList)

-- How tall should the whole histogram be? This function will determine the height 
findMaxFreq :: [Point] -> Int 
findMaxFreq [] = 0
findMaxFreq (x:xs)
  | (findMaxFreq xs) > (getFreq x) = findMaxFreq xs 
  | otherwise = getFreq x 


makeStars :: (a -> Bool) -> a -> Char 
makeStars cond x = if cond x then '*' else ' '

counts :: [Int] -> [Int]
counts = flip map [0..9] . (. (==)) . (length .) . flip filter

line :: [Int] -> Int -> String 
line someCounts n = map (makeStars (>=n)) someCounts
--}


{--
-- Horizontal histogram 
plotPoints :: [Point] -> [String]
plotPoints [] = ["\n==========\n0123456789\n"]
plotPoints pairs = map plotPoint pairs 
                  where
                       plotPoint (num, freq) = "\n" ++ replicate freq '*'

rotate :: [[a]] -> [[a]]
rotate = reverse . transpose 

rectangular :: a -> [[a]] -> [[a]]
rectangular padding rows = rectangle where
  width = maximum $ map length rows
  long_rows = map (++ repeat padding) rows
  rectangle = map (take width) long_rows

verticalHist :: [Int] -> [String]
verticalHist stuff = rotate $ rectangular ' ' (plotPoints $ obtainPoints stuff)

printVertHist :: Show a => [a] -> [String] -> IO ()
printVertHist xs columns = do mapM_ putStrLn columns
                              putStrLn $ map (const '=') xs
                              putStrLn $ concat $ map show xs 

--}
