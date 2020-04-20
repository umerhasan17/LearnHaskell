import Data.Array

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
tabulate2d (m, n) f = array ((0, 0), (n, m)) [((x, y), (f (x, y))) | x <- range(0, m), y <- range(0, n)]

tabulate:: (Enum i, Ix i) => (i, i) -> (i -> a) -> Array i a
tabulate (m, n) f = array (m, n) [(i, f i) | i <- range(m, n)]

dist':: String -> String -> Int
dist' xs ys
    -- = table
    = table ! (length xs, length ys)
    where
        table = tabulate2d (length xs, length ys) mdist
        mdist:: (Int, Int) -> Int
        mdist (0, 0)  = 0
        mdist (0, x) = x
        mdist (x, 0) = x
        mdist (x, y) 
            | sameChar  = table ! (x-1, y-1)
            | otherwise = 1 + minimum options
            where
                sameChar = (xs !! (x-1)) == (ys !! (y-1))
                options = [table ! (x-1, y-1), table ! (x, y-1), table ! (x-1, y)]

--   dist [] []
--     = 0
-- dist [] xs
--     = length xs
-- dist xs []
--     = length xs
-- dist (x: xs) (y: ys)
--     | x == y    = dist xs ys
--     | otherwise = 1 + minimum options
--     where
--         options = [op1, op2, op3]
--         op1 = dist xs (y: ys)
--         op2 = dist (x: xs) ys
--         op3 = dist xs ys

--             | index == 0  = 0
--             | index < length xs          = index
--             | index `mod` (length xs+1) == 0 = index - (xIndex * length xs)
--             | charsEqual                 = op3
--             | otherwise                  = 1 + minimum options
--             where
--                 (xIndex, yIndex) = divMod index (length xs)
--                 charsEqual = xIndex > 0 && yIndex > 0 && xs !! (xIndex-1) == ys !! (yIndex-1)
--                 options = [op1, op2, op3]
--                 -- left most column case already accounted for
--                 op1 = table ! (index - 1) 
--                 op2 = table ! (index - length xs)
--                 op3 = table ! (index - (length xs + 1) - 1)

        -- mdist [] []
        --     = 0
        -- mdist [] xs
        --     = length xs
        -- mdist xs []
        --     = length xs
        -- mdist (x: xs) (y: ys)
        --     | x == y    = table ! length xs * (length ys + 1)
        --     | otherwise = 1 + minimum options
        --     where
        --         options = [op1, op2, op3]
        --         op1 = table ! length xs * (length (y: ys) + 1)
        --         op2 = table ! length (x: xs) * (length ys + 1)
        --         op3 = table length xs * (length ys + 1)