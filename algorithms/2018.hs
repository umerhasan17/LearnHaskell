import Data.Array
import Data.List
import Data.Maybe

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

findDiffering:: [Int] -> [Int] -> Int
findDiffering as []
    = 0
findDiffering as bs
    = findDiffering' as bs 0
    where
        findDiffering':: [Int] -> [Int] -> Int -> Int
        findDiffering' as [] offset = offset
        findDiffering' as bs offset
            | ma == mb  = findDiffering' righta rightb (offset+mid+1)
            | ma < mb   = findDiffering' lefta leftb offset
            | otherwise = offset + mid
            where
                mid = (length bs) `div` 2
                ma = as !! mid
                mb = bs !! mid
                lefta = take (mid+1) as
                leftb = take (mid+1) bs
                righta = drop (mid+1) as
                rightb = drop (mid+1) bs

findDifferingN:: [Int] -> [Int] -> Int
findDifferingN as bs
    | length diff == 0 = -1
    | otherwise        = index
    where
        diff  = as \\ bs
        index = fromJust (findIndex (==(diff!!0)) as)

tabulate2d:: (Num i, Enum i, Ix i) => (i, i) -> ((i, i) -> a) -> Array (i, i) a 
tabulate2d (m, n) f = array ((0, 0), (n, m)) [((x, y), (f (x, y))) | x <- range(0, m), y <- range(0, n)]

lpsubseq:: String -> Int
lpsubseq s = table ! (0, l-1)
    where
        l = length s
        table = tabulate2d (l-1, l-1) lpsubseq'
        lpsubseq':: (Int, Int) -> Int
        lpsubseq' (x, y)
            | x == y           = 1
            | y < x            = -1
            | same && y == x+1 = 2
            | same && innerP   = (table ! (x+1, y-1)) + 2
            | otherwise        = maximum [(table ! (x+1, y)), (table ! (x, y-1))]
            where
                same  = (s !! x) == (s !! y)
                inner = (table ! (x+1, y-1))
                innerP = inner == ((y-1) - (x+1)) + 1


-- nkoonj table

-- (0,0),(0,1),(0,2),(0,3),(0,4),(0,5) -  1 1 1 
-- (1,0),(1,1),(1,2),(1,3),(1,4),(1,5) -  -1 1 1 
-- (2,0),(2,1),(2,2),(2,3),(2,4),(2,5) -
-- (3,0),(3,1),(3,2),(3,3),(3,4),(3,5) -
-- (4,0),(4,1),(4,2),(4,3),(4,4),(4,5) -
-- (5,0),(5,1),(5,2),(5,3),(5,4),(5,5) -

kthLargest:: Int -> [Int] -> Int
kthLargest k [] = undefined
kthLargest k xs@(x: xs') 
    | l == k    = minimum xs
    | same      = kthLargest k xs'
    | lr < k    = kthLargest (k - lr) left 
    | lr == k   = minimum right
    | lr > k    = kthLargest k (tail right) 
    | otherwise = undefined
    where 
        l    = length xs
        lr   = length right
        (left, right) = splitByHead xs
        same = (length left) == (length xs)

splitByHead []        = ([], [])
splitByHead xs@(x: _) = (filter (<x) xs, filter (>= x) xs) 

asubseq:: [Int] -> [Int] -> Bool
asubseq [] _ = True
asubseq _ [] = False
asubseq (a:as) (b: bs)
    | a == b    = checkTail as bs || r
    | otherwise = r
    where
        r = asubseq (a:as) bs
        checkTail:: [Int] -> [Int] -> Bool
        checkTail as bs = as == (take (length as) bs)