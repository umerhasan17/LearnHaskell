data Shape = Triangle Float Float Float | Square Float | Circle Float

-- data Triangle = Float Float Float

-- how do I call this function
area :: Shape -> Float
area (Triangle a b c)
  = triangleArea a b c
area (Square d)
  = d * d
area (Circle r)
  = pi * r ^ 2

-- why can I not replace these with a a a a
-- triangleArea :: a -> a -> a -> a
triangleArea :: Float -> Float -> Float -> Float
triangleArea a b c
  = sqrt (s * (s - a) * (s - b) * (s - c))
  where
    s = (a + b + c) / 2


data Date = Day Month Year

type Day = Int

type Month = Int

type Year = Int

-- age :: Date -> Date -> Int
-- age d1 d2
--   = d1.Year - d2.Year

flatten :: Tree a -> [a]
flatten Empty
  = []
flatten (Node t1 x t2)
  = flatten t1 ++ (x : flatten t2)

data Tree = Leaf | Node Tree Tree
          deriving (Eq, Show)

makeTrees :: Int -> [Tree]
makeTrees n
  | n == 0    = [Leaf]
  | otherwise = 