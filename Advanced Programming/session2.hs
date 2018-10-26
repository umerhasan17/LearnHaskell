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
-- cannot be done without lazy evaluation 
-- however can be done with dynamic programming?

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

fibs''
  = fibTable

-- number of steps required to solve hanoi is 2^n - 1
hanoi3 :: Int -> Int -> Int -> Int -> [(Int, Int)]
-- from to via 
hanoi3 0 f t v 
  = []
hanoi3 n f t v 
  = hanoi3 (n - 1) f v t ++ 
    [(f, t)] ++
    hanoi3 (n - 1) v t f 
  -- to is the via 

-- it is believed that the following is optimal for n = 4
-- hasn't been proven
-- set aside k sorted elements to spare peg
-- do hanoi 3 with the rest of the elements
-- use the 4 strategy to get the k sorted elements onto the peg 

hanoi4 :: Int -> Int -> Int -> Int -> Int -> [(Int, Int)]
-- from to via 
hanoi4 0 f t v v'
  = []
hanoi4 n f t v v'
  = hanoi4 k f v' t v ++
    hanoi3 (n - k) f t v ++
    hanoi4 k v' t f v
  where
    k = snd(ks !! n)

ks :: [(Int, Int)]
ks
  = (0, 0) : (1, 0) : map makePair [2..]
  where
    makePair n = minimum [ (cost n k, k) | k <- [0 .. n -1]]
    cost n k   = 2 * fst (ks !! k) + 2 ^ (n - k) - 1

-- [length ( hanoi4 n 1 2 3 4) | n <- [1..10]]
-- [1,3,5,9,13,17,25,33,41,49]
