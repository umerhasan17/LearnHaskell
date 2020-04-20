fib :: Int -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

mfib :: Int -> Int
mfib n = fib' n 0 1
    where
        fib':: Int -> Int -> Int -> Int
        fib' 0 x _ = x 
        fib' n x y = fib' (n-1) (y) (x+y)


fib'':: Int -> Int
fib'' 0 = 0
fib'' n
    = round ( phi n / sqrt 5)
    where
        phi:: Int -> Double
        phi 1 = (1 + sqrt 5) / 2
        phi n
            | even n    = k ** 2
            | otherwise = (k ** 2) * (phi 1)
            where
                k = phi (n `div` 2)

-- mkStdGen :: Int → StdGen
-- random :: Random a ⇒ StdGen → (a, StdGen)
-- randomR :: Random a ⇒ (a, a) → StdGen → (a, StdGen)