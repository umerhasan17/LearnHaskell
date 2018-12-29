-- Binomial Heaps, easiest test so far

type BinHeap a = [BinTree a]

data BinTree a = Node a Int (BinHeap a)
               deriving (Eq, Ord, Show)

--------------------------------------------------------------
-- PART I

value :: BinTree a -> a
value (Node value _ _)
  = value

rank :: BinTree a -> Int
rank (Node _ rank _)
  = rank

children :: BinTree a -> [BinTree a]
children (Node _ _ children)
  = children

combineTrees :: Ord a => BinTree a -> BinTree a -> BinTree a
combineTrees t1@(Node v1 r1 c1) t2@(Node v2 r2 c2)
  | v1 < v2   = (Node v1 (r1 + 1) (t2 : c1))
  | otherwise = (Node v2 (r2 + 1) (t1 : c2))

--------------------------------------------------------------
-- PART II

extractMin :: Ord a => BinHeap a -> a
extractMin (x : xs)
  = extractMin' (x : xs) v
  where
    (Node v _ _) = x
    extractMin' :: Ord a => BinHeap a -> a -> a
    extractMin' (x : xs) min
      | v1 < min  = extractMin' xs v1
      | otherwise = extractMin' xs min
      where
        (Node v1 _ _) = x
    extractMin' [] min
      = min

mergeHeaps :: Ord a => BinHeap a -> BinHeap a -> BinHeap a
mergeHeaps [] h
  = h
mergeHeaps h []
  = h
mergeHeaps h1 h2
  | r1 < r2   = (t1 : mergeHeaps ts1 h2)
  | r2 < r1   = (t2 : mergeHeaps h1 ts2)
  | otherwise = mergeHeaps combined merged
  where
    (t1 : ts1)      = h1
    (t2 : ts2)      = h2
    (Node v1 r1 c1) = t1
    (Node v2 r2 c2) = t2
    combined        = [combineTrees t1 t2]
    merged          = mergeHeaps ts1 ts2

insert :: Ord a => a -> BinHeap a -> BinHeap a
insert v h
  = mergeHeaps [(Node v 0 [])] h

deleteMin :: Ord a => BinHeap a -> BinHeap a
deleteMin h
  | min == v1 = mergeHeaps (reverse c1) ts1
  | otherwise = deleteMin ts1
  where
    min = extractMin h
    (t1 : ts1) = h
    (Node v1 r1 c1) = t1

remove :: Eq a => a -> BinHeap a -> BinHeap a
remove
  = undefined

removeMin :: Ord a => BinHeap a -> (BinTree a, BinHeap a)
removeMin
  = undefined

binSort :: Ord a => [a] -> [a]
binSort xs
  = toList (toHeap xs [])
  where
    toHeap :: Ord a => [a] -> BinHeap a -> BinHeap a
    toHeap [] h
      = h
    toHeap (x : xs) h
      = (insert x h) ++ toHeap xs h
    toList :: Ord a => BinHeap a -> [a]
    toList []
      = []
    toList h
      = (min : toList h')
      where
        min = extractMin h
        h'  = deleteMin h


--------------------------------------------------------------
-- PART III

toBinary :: BinHeap a -> [Int]
toBinary
  = undefined

binarySum :: [Int] -> [Int] -> [Int]
binarySum
  = undefined

------------------------------------------------------
-- Some sample trees...

t1, t2, t3, t4, t5, t6, t7, t8 :: BinTree Int
-- Note: t7 is the result of merging t5 and t6

-- t1 to t4 appear in Figure 1...
t1 = Node 4 0 []
t2 = Node 1 1 [Node 5 0 []]
t3 = Node 2 2 [Node 8 1 [Node 9 0 []], 
               Node 7 0 []]
t4 = Node 2 3 [Node 3 2 [Node 6 1 [Node 8 0 []], 
                         Node 10 0 []],
               Node 8 1 [Node 9 0 []],
               Node 7 0 []]

-- t5 and t6 are on the left of Figure 2; t7 is on the
-- right
t5 = Node 4 2 [Node 6 1 [Node 8 0 []], 
                         Node 10 0 []]
t6 = Node 2 2 [Node 8 1 [Node 9 0 []], Node 7 0 []]
t7 = Node 2 3 [Node 4 2 [Node 6 1 [Node 8 0 []], Node 10 0 []],
               Node 8 1 [Node 9 0 []], 
               Node 7 0 []]

-- An additional tree...
t8 = Node 12 1 [Node 16 0 []]

------------------------------------------------------
-- Some sample heaps...

h1, h2, h3, h4, h5, h6, h7 :: BinHeap Int
-- Two arbitrary heaps for testing...
h1 = [t2, t7]
h2 = [Node 1 2 [Node 12 1 [Node 16 0 []],
                Node 5 0 []],
      Node 2 3 [Node 4 2 [Node 6 1 [Node 8 0 []],
                          Node 10 0 []],
                Node 8 1 [Node 9 0 []],
                Node 7 0 []]]

-- h3 is shown in Figure 3...
h3 = [t1, t2, t4]

-- Two additional heaps, used below. They are shown
-- in Figure 4(a)...

h4 = [t2, t5]
h5 = [t1, t8]

-- h6 is the result of merging h4 and h5, shown in Figure 4(b)...
h6 = [Node 4 0 [],
      Node 1 3 [Node 4 2 [Node 6 1 [Node 8 0 []],
                          Node 10 0 []],
                Node 12 1 [Node 16 0 []],
                Node 5 0 []]]

-- h7 is shown in Figure 5...
h7 = [Node 4 3 [Node 4 2 [Node 12 1 [Node 16 0 []],
                          Node 5 0 []],
                Node 6 1 [Node 8 0 []],
                Node 10 0 []]]


