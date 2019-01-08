import Data.Char

function x y
  = mod y x

depunctuate :: String -> String
depunctuate
  = filter (not . flip elem ".,:")

-- makeString
--   = map chr

enpower :: [Int] -> Int
enpower
  = foldr1 (flip (^))


revAll :: [[a]] -> [a]
revAll
  = concatMap reverse

rev :: [a] -> [a]
rev xs
  = foldl (flip (:)) [] xs

dezip :: [(a, b)] -> ([a], [b])
dezip
  = undefined

squash :: (a -> a -> b) -> [a] -> [b]
squash f xs
  = zipWith f xs (tail xs)

map :: (a -> b) -> [a] -> [b]
map f
  = foldr g []
  where
    g x ys = f x : ys
