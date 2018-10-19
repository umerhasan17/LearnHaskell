-- doesn't finish but can take first 10 values due to lazy evaluation
ones
  = 1 : ones

-- two fibs algorithms using zipWith
fibs
  = s
  where
    s = 0 : s'
    s' = 1 : s''
    s'' = zipWith (+) s s'

fibs'
  = 0 : 1 : zipWith (+) fibs' (tail fibs')

-- this feels like a circular dependency
-- however base cases in fib' solve this
-- so the index in fibtable at position 0 and 1 exist

fibTable :: [Int]
fibTable
  = map fib' [0..]

fib' 0 
  = 0
fib' 1
  = 1
-- don't call fib prime here because this causes exponential computation
-- instead index the fib table
fib' n
  = fibTable !! (n - 1) + fibTable !! (n - 2)
