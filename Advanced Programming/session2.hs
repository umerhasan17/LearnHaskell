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


