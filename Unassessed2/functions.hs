type Vertex = (Float, Float)

-- Question 3
distance :: Vertex -> Vertex -> Float
-- takes vertex tuples and returns the distance between them
-- uses Pythagoras
distance (x1 , y1) (x2 , y2)
  = sqrt((x1 - x2) ^ 2 + (y1 - y2) ^ 2)

-- Question 6
fact :: Int -> Int
-- Pre: x >= 0
-- maximum factorial is 20! before overflow occurs
fact x
  | x == 0    = 1
  | otherwise = x * fact (x-1)

-- Question 15
fib :: Int -> Int
-- Think about improving this function 
-- There are a lot of redundant calls made
fib x
  | x == 0    = 0
  | x == 1    = 1
  | otherwise = fib (x-1) + fib (x-2)
