import Data.List

pos :: Int -> [Int] -> Int
pos _ []
  = error "Index out of range"
pos index (x : xs)
  | index == 1 = x
  | otherwise  = pos (index - 1) xs

pos' :: Int -> [Int] -> Int
pos' i (x: xs)
  | i == x    = 0
  | otherwise = 1 + pos' i xs

-- rev :: [a] -> [a]
rev []
  = []
rev (x : xs)
  = rev xs ++ x

-- List Comprehensions

findAll x t
  = [y | (x', y) <- t, x == x']

remove :: Eq a => a -> [(a, b)] -> [(a, b)]
remove x table
  = filter ( (/=x).fst) table
  where
    -- p      = 
    toDrop = [y | y@(x', _) <- table, x' /= x]

quicksort :: (Eq a) => (Ord a) => [a] -> [a]
quicksort []
  = []
quicksort (x : xs)
  = lesser ++ (x : greater)
  where
    lesser  = quicksort (filter (<=x) xs)
    greater = quicksort (filter (>x) xs)

splitUp :: String -> [String]
splitUp "" = []
splitUp s
  = w : splitUp ws
  where
    (w, ws) = nextWord (removeWhitespace s)

removeWhitespace :: String -> String
removeWhitespace ""
  = ""
removeWhitespace s@(c : cs)
  | isSpace c = removeWhitespace cs
  | otherwise = s

isSpace :: Char -> Bool
isSpace c
  = (c == ' ')

nextWord :: String -> (String, String)
--Pre: The first character is non-whitespace
nextWord ""
  = ("", "")
nextWord (c : cs)
  | isSpace c = ("", cs)
  | otherwise = (c : w, s)
  where
    (w, s) = nextWord cs

allSplits :: [a] -> [([a], [a])]
allSplits xs
  = [splitAt x xs | x <- [1..length(xs) - 1]]

prefixes :: [t] -> [[t]]
prefixes xs
  = [take n xs | n <- [1..length(xs)]] 

substrings :: String -> [String]
substrings []
  = []
substrings xs
  = prefixes xs ++ substrings (tail xs)

-- perms :: [a] -> [[a]]
-- perms xs
--   = 

-- tails :: [a] -> [[a]]
-- tails xs
--  = [xs]

substrings2 :: String -> [String]
substrings2 s
  = [i | t <- tails s, i <- tail (inits t)]