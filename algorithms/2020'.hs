import Data.Array

tabulate:: (Enum i, Ix i) => (i, i) -> (i -> a) -> Array i a
tabulate (m, n) f = array (m, n) [(i, f i) | i <- range(m, n)]

tabulate2d:: (Num i, Enum i, Ix i) => (i, i) -> ((i, i) -> a) -> Array (i, i) a
tabulate2d (m, n) f = array ((0, 0), (m, n)) [((x, y), (f (x, y))) | x <- range(0, m), y <- range(0, n)]

catalan :: Int -> Integer
catalan 0 = 1
catalan n = sum [catalan i * catalan (n-i-1) | i <- [0 .. n-1]]

catalan':: Int -> Integer
catalan' n = table ! n 
    where
        table = tabulate (0, n+1) memo
        memo:: Int -> Integer
        memo 0 = 1
        memo n = sum [(table ! i) * (table ! (n-i-1)) | i <- [0..n-1]]

chain:: Array Int Int -> (Int, Int) -> Int
chain a (i, j)   
    | i == j      = 0
    | otherwise   = minimum xs
    where
        xs = [chain a (i, x) + chain a (x+1, j) + (ops a x) | x <- [i..j-1]]
        ops:: Array Int Int -> Int -> Int
        ops a i = (a!i) * (a!(i+1)) * (a!(i+2))


chain':: Array Int Int -> (Int, Int) -> Int
chain' a (i, j)
    = table ! (i, j)
    where
        table = tabulate2d (i+1, j+1) memo
        memo:: (Int, Int) -> Int
        memo (i, j)
            | i == j    = 0
            | otherwise = minimum xs
            where
                xs = [table ! (i, x) + table ! (x+1, j) + (ops a x) | x <- [i..j-1]]
                ops:: Array Int Int -> Int -> Int
                ops a i = (a!i) * (a!(i+1)) * (a!(i+2))
