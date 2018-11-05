getClosing :: Char -> Char
getClosing '(' = ')'
getClosing  c  = c

isOpening :: Char -> Bool
isOpening c
  | c == '('  = True
  | otherwise = False

checkBrackets :: String -> Bool
checkBrackets str
  = check str []
  where
    check :: String -> String -> Bool
    check [] []
      = True
    check [] (top : bottom)
      = False
    check (x : xs) []
      | not (isOpening x) = False
      | otherwise         = check xs [x]
    check (x : xs) stack@(top : bottom)
      | isOpening x         = check xs ((getClosing x) : stack)
      | getClosing x == top = check xs bottom
      
     
