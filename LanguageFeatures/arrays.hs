import Data.Array

-- create a 1d array
myArray :: Array Int Char
myArray = listArray (0, 2) ['a', 'b', 'c']

-- this is list array
-- listArray :: Ix i => (i, i) -> [e] -> Array i e
--         -- Defined in ‘GHC.Arr’
-- Ix are index types requires range of values between x and y to be finite: