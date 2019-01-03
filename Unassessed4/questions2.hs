import Data.Char

function x y
  = mod y x

depunctuate :: String -> String
depunctuate
  = filter (not . flip elem ".,:")

makeString
  = map chr

enpower ns
    = foldr1 (flip (^)) ns 

-- enpower
--   = foldr1 (flip (^))