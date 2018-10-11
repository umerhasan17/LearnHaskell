import Data.List

perm :: [Int] -> [[Int]]
perm []
  = [[]]
perm xs
  = [x: ys | x <- xs , ys <- perm(xs \\ [x])]