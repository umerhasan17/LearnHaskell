import Data.Array

tabulate:: (Enum i, Ix i) => (i, i) -> (i -> a) -> Array i a
tabulate (m, n) f = array (m, n) [(i, f i) | i <- [m..n]]

fib:: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

fib':: Int -> Int
fib' n = table ! n
    where
        table = tabulate (0, n) mfib
        mfib:: Int -> Int
        mfib 0 = 0
        mfib 1 = 1
        mfib n = table ! (n-1) + table ! (n-2)

coins:: Int -> [Int] -> Int
coins 0 _ = 0
coins target cs
    | target < 0 = -1
    | otherwise  = minimum (filter f [1 + (coins i cs) | i <- nexts])
    where
        nexts = filter f [(target-i) | i <- cs]
        f x = x > -1

coins':: Int -> [Int] -> Int
coins' target cs = table ! target
    where
        table = tabulate (0, target) mcoins
        mcoins:: Int -> Int
        mcoins 0 = 0
        mcoins cur
            | cur < 0  || length nexts == 0 = -1
            | otherwise                     = minimum (filter f [1 + table ! n | n <- nexts])
            where
                nexts = filter f [(cur-i) | i <- cs]
                f x = x > -1