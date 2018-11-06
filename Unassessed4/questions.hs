allSame :: [Int] -> Bool
allSame [x]
  = True
allSame (x : xs)
  = (x == (xs !! 0)) && allSame xs

allSame2 :: [Int] -> Bool
allSame2 xs
  -- and together the list of bools coming out
  -- zip together xs and tail xs
  -- given the function == which returns True if they are both the same
  = and (zipWith (==) xs (tail xs))

squash :: (a -> a -> b) -> [a] -> [b] 
squash f xs
  = zipWith f xs (tail xs)

converge :: (a -> a -> Bool) -> [a] -> a
converge f [x]
  = x
converge f (x : y : xs)
  | f x y     = y
  | otherwise = converge f (y : xs)

limit :: (a -> a -> Bool) -> [a] -> [a]
limit f [x]
  = [x]
limit f (x : y : ys)
  | f x y     = [x , y]
  | otherwise = (x : limit f (y : ys))

-- why does this not compile
map2 :: (a -> b) -> [a] -> [b]
map2 f 
  = foldr g []
  where
    g x ys = f x : ys
  
repeatUntil :: (a -> Bool) -> (a -> a) -> a -> a 
repeatUntil condition f x
  | condition x = x
  | otherwise   = repeatUntil condition f (f x)

-- why is there no argument for x
-- how is the return value being parsed with the dots and functions. 
repeatUntil2 :: (a -> Bool) -> (a -> a) -> a -> a 
repeatUntil2 p f x
  = (head . filter p . iterate f) x

  
-- addOne :: Int -> Int -> (Int, Int)
-- addOne
--   = ((+1), (+1))

--   ((Int -> Int), (Int -> Int))

any' p xs
  = or (map p xs)

-- any' p
--   = or . map p

all' p = and . map p


