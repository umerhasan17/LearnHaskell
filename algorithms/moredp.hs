import Data.Array

tabulate2d:: (Num i, Enum i, Ix i) => (i, i) -> ((i, i) -> a) -> Array (i, i) a
tabulate2d (m, n) f = array ((0, 0), (m, n)) [((x, y), (f (x, y))) | x <- range(0, m), y <- range(0, n)]

-- canIWin:: Int -> Int -> Int
-- canIWin m d
--     = table ! d
--     where
--         table = tabulate2d d memo
--         memo :: (Int, Int) -> Bool
--         memo (m, d)
--             | d <= m    = true
--             | otherwise = any([table ! (d-x-y) | x <- [1..m], y <- [1..m]])

tabulate:: (Enum i, Ix i) => (i, i) -> (i -> a) -> Array i a
tabulate (m, n) f = array (m, n) [(i, f i) | i <- range(m, n)]

coinChange:: Int -> [Int] -> Int
coinChange m cs
    = table ! m
    where
        table = tabulate (0, m) memo
        memo:: Int -> Int
        memo m
            | m == 0    = 0 
            | elem m cs = 1
            | otherwise = minimum [1 + table ! (m-x) | x <- (filter (<m) cs)]

coinChange2:: Int -> [Int] -> Int
coinChange2 m cs
    = table ! (m, l)
    where
        l = length cs
        table = tabulate2d (m+1, l+1) memo
        memo:: (Int, Int) -> Int
        memo (0, _) = 1
        memo (_, 0) = 0
        memo (m', l')
            | biggestCoin <= m'  = table ! (m', l'-1) + table ! (m'-biggestCoin, l')
            | otherwise          = table ! (m', l'-1)
            where
                biggestCoin = cs !! (l'-1)
        -- memo m = sum [table ! (m-x) + table ! (x) | x <- (filter (<=m) cs)]

        