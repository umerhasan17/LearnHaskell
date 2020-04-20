-- find closest number in sorted array
findClosest :: Int -> [Int] -> Int
findClosest n xs
    | length xs == 0 = n
    | otherwise      = findClosest' n xs (xs !! 0)
    where
        findClosest':: Int -> [Int] -> Int -> Int
        findClosest' n xs cur
            | l == 1 && check1 = (xs !! 0)
            | l == 1           = cur
            | n == m           = n
            | n < m && check   = findClosest' n left m
            | n < m            = findClosest' n left cur
            | n > m && check   = findClosest' n right m
            | n > m            = findClosest' n right cur
            where        
                l = length xs
                m = xs !! (l `div` 2)
                check1 = getMagnitude (n-(xs!!0)) < getMagnitude (n-cur)
                check = getMagnitude (n-m) < getMagnitude (n-cur)
                left = (take (l `div` 2) xs)
                right = (drop (l `div` 2) xs) 

getMagnitude n
    | n < 0     = negate (n)
    | otherwise = n

-- log n 



