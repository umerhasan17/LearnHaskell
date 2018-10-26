pos :: Int -> [Int] -> Int
pos _ []
  = error "Index out of range"
pos index (x : xs)
  | index == 1 = x
  | otherwise  = pos (index - 1) xs

pos' :: Int -> [Int] -> Int
pos' i (x: xs)
  | i == x    = 0
  | otherwise = 1 + pos' i xs

-- rev :: [a] -> [a]
rev []
  = []
rev (x : xs)
  = rev xs ++ x
