data SuffixTree = Leaf Int | Node [(String, SuffixTree)] 
                deriving (Eq, Show)

------------------------------------------------------

isPrefix :: String -> String -> Bool
isPrefix [] []
  = True
isPrefix [] (y : ys)
  = True
isPrefix (x : xs) []
  = False
isPrefix (x : xs) (y : ys)
  = (x == y) && isPrefix xs ys

removePrefix :: String -> String -> String
--Pre: s is a prefix of s'
removePrefix [] ys
  = ys
removePrefix (x : xs) (y: ys)
  = removePrefix xs ys

suffixes :: [a] -> [[a]]
suffixes xs
  = suffixes' xs []
  where
    suffixes' :: [a] -> [[a]] -> [[a]]
    suffixes' [] acc
      = acc
    suffixes' (x : xs) acc
      = acc ++ [(x : xs)] ++ suffixes' xs acc

isSubstring :: String -> String -> Bool
isSubstring s s'
  = isSubstring' s xs
  where
    xs = suffixes s'
    isSubstring' :: String -> [String] -> Bool
    isSubstring' s []
      = False
    isSubstring' s (x : xs)
      = isPrefix s x || isSubstring' s xs

findSubstrings :: String -> String -> [Int]
findSubstrings s s'
  = findSubstrings' s xs 0
  where
    xs = suffixes s'
    findSubstrings' :: String -> [String] -> Int -> [Int]
    findSubstrings' s [] index
      = []
    findSubstrings' s (x : xs) index
      | isPrefix s x = index : recurse
      | otherwise    = recurse
      where
        recurse = findSubstrings' s xs (index + 1)

------------------------------------------------------

getIndices :: SuffixTree -> [Int]
getIndices tree
  = getIndices' [tree] []
  where
    getIndices' :: [SuffixTree] -> [Int] -> [Int]
    getIndices' [] indices
      = indices
    getIndices' ((Leaf n) : xs) indices
      = n : getIndices' xs indices ++ indices
    getIndices' ((Node x) : xs) indices
      = getIndices' trees indices ++ getIndices' xs indices ++ indices
      where
        trees = snd (unzip x)

partition :: Eq a => [a] -> [a] -> ([a], [a], [a])
partition 
  = undefined

findSubstrings' :: String -> SuffixTree -> [Int]
findSubstrings'
  = undefined

------------------------------------------------------

insert :: (String, Int) -> SuffixTree -> SuffixTree
insert 
  = undefined

-- This function is given
buildTree :: String -> SuffixTree 
buildTree s
  = foldl (flip insert) (Node []) (zip (suffixes s) [0..])

------------------------------------------------------
-- Part IV

longestRepeatedSubstring :: SuffixTree -> String
longestRepeatedSubstring 
  = undefined

------------------------------------------------------
-- Example strings and suffix trees...

s1 :: String
s1 
  = "banana"

s2 :: String
s2 
  = "mississippi"

t1 :: SuffixTree
t1 
  = Node [("banana", Leaf 0), 
          ("a", Node [("na", Node [("na", Leaf 1), 
                                   ("", Leaf 3)]), 
                     ("", Leaf 5)]), 
          ("na", Node [("na", Leaf 2), 
                       ("", Leaf 4)])]

t2 :: SuffixTree
t2 
  = Node [("mississippi", Leaf 0), 
          ("i", Node [("ssi", Node [("ssippi", Leaf 1), 
                                    ("ppi", Leaf 4)]), 
                      ("ppi", Leaf 7), 
                      ("", Leaf 10)]), 
          ("s", Node [("si", Node [("ssippi", Leaf 2), 
                                   ("ppi", Leaf 5)]), 
                      ("i", Node [("ssippi", Leaf 3), 
                                  ("ppi", Leaf 6)])]), 
          ("p", Node [("pi", Leaf 8), 
                      ("i", Leaf 9)])]

