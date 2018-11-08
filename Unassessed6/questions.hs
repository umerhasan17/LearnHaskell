-- how to find list of all functions in GHC.Enum
-- derive specific functions in classes
-- 

data Colour = Red | Green | Blue
            deriving (Show, Bounded)

instance Bounded Colour where
  minBound colour = Red
