import Data.Array

fun (x:y:[]) = True
fun _ = False

-- pi = undefined

-- splitBy2:: [Int] -> ([Int], [Int])
-- splitBy2 xs = splitAt (length xs `div` 2) xs

-- closest':: [Int] -> (Int, Int) 
-- closest' (x:y:[]) = (x, y)
-- closest' (x:y:z:[]) = pi (x, y) (y, z)
-- closest' xs = pi (last ys, head zs)  (pi (closest' ys) (closest' zs))
--     where
--         (ys, zs) = splitBy2 xs


data AList a = AList Int Int (Array Int a)

head:: AList a -> a 
head (AList s e arr) = arr ! s

last:: AList a -> a 
last (AList s e arr) = arr ! e

split:: AList a -> (AList a, AList a)
split (AList s e arr) = ((AList s mid arr), (AList (mid+1) e arr))
    where
        mid = (s+e) `div` 2

closest'':: AList Int -> (Int, Int)
closest'' a@(AList s e arr)
    | e == s+1 = (arr!s, arr!e)
    | e == s+2 = pi (arr!s, arr!(s+1)) (arr!(s+1), arr!(e))
    | otherwise = pi (arr1!e1, arr2!s2)  (pi (closest'' b) (closest'' c))
        where
            (b@(AList s1 e1 arr1), c@(AList s2 e2 arr2)) = split a

