-- how to find list of all functions in GHC.Enum
-- derive specific functions in classes
-- 

data Colour = Red | Green | Blue
            deriving (Show, Bounded, Enum)

-- instance Bounded Colour where
--   minBound colour = Red

-- find all functions in enum

-- Question 2
data Time = T24 Int Int | THM Int Int AmPm

data AmPm = Am | Pm

to24 :: Time -> Time
to24 (THM 12 m Am)
  = (T24 00 m)
to24 (THM 12 m Pm)
  = (T24 12 m)
to24 (THM h m Am)
  = (T24 h m)
to24 (THM h m Pm)
  = (T24 (h + 12) m)
to24 t 
  = t

equalTime :: Time -> Time -> Bool
equalTime t t'
  = eqTime (to24 t) (to24 t')
  where
    eqTime :: Time -> Time -> Bool
    eqTime (T24 h m) (T24 h' m')
      = (h == h') && (m == m')

instance Eq Time where
   (==) = equalTime

instance Show AmPm where
  show (Am) = "am"
  show (Pm) = "pm"

instance Show Time where
  show (THM 12 0 Am)   = "Midnight"
  show (THM 12 0 Pm)   = "Midday"  
  show (T24 h m)   = show' h ++ show' m ++ "HRS"
  show (THM h m t) = show' h ++ ":" ++ show' m ++ show t

show' x
  | x < 10    = '0' : show x
  | otherwise = show x