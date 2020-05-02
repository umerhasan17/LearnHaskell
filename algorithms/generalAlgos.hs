import Data.Array
import Data.List

-- smallest free natural number given a list
-- this solution is n^2 
minFree:: [Int] -> Int
minFree xs = head $ [0..] \\ xs
