data Shape = Triangle Float Float Float | Square Float | Circle Float

-- how do I call this function
area :: Shape -> Float
area (Triangle a b c)
  = triangleArea a b c
area (Square d)
  = d * d
area (Circle r)
  = pi * r ^ 2

-- why can I not replace these with a a a a
triangleArea :: Float -> Float -> Float -> Float
triangleArea a b c
  = sqrt (s * (s - a) * (s - b) * (s - c))
  where
    s = (a + b + c) / 2