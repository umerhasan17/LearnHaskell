import Data.Array
import Data.List
import Data.Ord

-- Question 1a

dist:: String -> String -> Int
dist [] []
    = 0
dist [] xs
    = length xs
dist xs []
    = length xs
dist (x: xs) (y: ys)
    | x == y    = dist xs ys
    | otherwise = 1 + minimum options
    where
        options = [op1, op2, op3]
        op1 = dist xs (y: ys)
        op2 = dist (x: xs) ys
        op3 = dist xs ys

-- replace = map dist (letter: xs) (y: ys) | letter  (['a'..'z'] \\ x) 

-- time complexity 3^(a+b)

-- Question 1b

tabulate2d:: (Num i, Enum i, Ix i) => (i, i) -> ((i, i) -> a) -> Array (i, i) a
tabulate2d (m, n) f = array ((0, 0), (m, n)) [((x, y), (f (x, y))) | x <- range(0, m), y <- range(0, n)]

tabulate:: (Enum i, Ix i) => (i, i) -> (i -> a) -> Array i a
tabulate (m, n) f = array (m, n) [(i, f i) | i <- range(m, n)]

dist':: String -> String -> Int
dist' xs ys
    = table ! (m, n)
    where
        m = length xs
        n = length ys
        table = tabulate2d (m+1, n+1) mdist
        mdist:: (Int, Int) -> Int
        mdist (0, 0)  = 0
        mdist (0, x) = x
        mdist (x, 0) = x
        mdist (x, y) 
            = minimum [
                (table ! (x-1, y)) + 1,
                (table ! (x, y-1)) + 1,
                (table ! (x-1, y-1)) + if sameChar then 0 else 1
            ]
            where
                sameChar = (xs !! (x-1)) == (ys !! (y-1))

dists:: String -> String -> [String]
dists [] []
    = []
dists [] xs
    = tail (inits xs)
    -- = [take i xs | i <- [0..length xs]]
dists xs []
    = tail (tails xs)
    -- = [take i xs | i <- [length xs, length xs - 1.. 0]]
dists xs@(x: xs') ys@(y: ys')
    -- = op3
    = minimums [op1, op2, op3]
    where
        op1 = xs' : dists xs' ys
        op2 = (dists xs ys') ++ [ys]
        -- op2 = map (y:) (dists xs ys')
        op3 = if x == y then map (x:) (dists xs' ys') else (map (y:) (xs':dists xs' ys'))
        minimums :: [[String]] -> [String]
        minimums xs
            = minimumBy (comparing length) xs

        

        minimums' [xs] = xs
        minimums' (xs:xss)
            | length xs < length ys = xs
            | otherwise             = ys
            where
                ys = minimums' xss
            -- hello tree --> h     ello tree
            -- hello -> [dists ello tree]
