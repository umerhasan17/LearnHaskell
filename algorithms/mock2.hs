import System.Random

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
-- creates a seed value given an integer

-- random :: Random a ⇒ StdGen → (a, StdGen)
-- generates a random value along with a new stdgen (uses default range determined by type)

-- randomR :: Random a ⇒ (a, a) → StdGen → (a, StdGen)
-- generates a number between low high range given and returns number with new stdgen

-- sqrt5:: Double
-- sqrt5 = sqrt5' (mkStdGen 42) 0 0
--     where
--         -- stores below, total
--         sqrt5' :: StdGen -> Int -> Int -> Double
--         sqrt5' s m n
--             | n == 100000   = 3 * (fromIntegral m / fromIntegral  n)
--             | (r ** 2) <= 5.0 = sqrt5' s' (m+1) (n+1) 
--             | otherwise     = sqrt5' s' m (n+1)
--             where
--                 (r, s') = randomR (0 3) s

root2 :: Double
root2 = loop (mkStdGen 42) 0 0
    where
        loop seed m n
            | n == 100000 = 2 * m / fromIntegral n
            | otherwise   = loop seed' m' n'
            where 
                n' = n+1
                m' = if inside x then m + 1 else m
                (x,seed') = randomR (0, 2) seed
                inside :: Double -> Bool
                inside x = x * x <= 2