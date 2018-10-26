-- Advanced Programming Session 3

import Data.List

data Tree a = Empty | Node (Tree a) a (Tree a)
              deriving (Eq, Show) 

---------------------------------------------------------------------------------
-- A couple of useful helper functions

isNode :: Tree a -> Bool
isNode Empty 
  = False
isNode t 
  = True

children :: Tree a -> [Tree a]
children Empty 
  = []
children (Node l x r) 
  = [l, r]

---------------------------------------------------------------------------------
-- Pre-order traversal

preWalk :: Tree a -> [a]
preWalk Empty 
  = []
preWalk (Node l x r) 
  = x : preWalk l ++ preWalk r

---------------------------------------------------------------------------------
-- Relabels a tree depth-first

labelDF :: Tree a -> Tree Int
labelDF t 
  = t'
  where
    (t', _) = labelTree t 1
    labelTree :: Tree a -> Int -> (Tree Int, Int)
    labelTree Empty n 
      = (Empty, n)
    labelTree (Node l x r) n 
      = (Node t' n t'', n'')
      where
        (t',  n' ) = labelTree l (n + 1) 
        (t'', n'') = labelTree r n'
                              
---------------------------------------------------------------------------------
-- Walks a tree using breadth-first traversal

bfWalk :: Tree a -> [a]
-- forests are defined as layers in trees
-- define the forest as a list, after going through the forest
-- add children to the end of the list 
bfWalk t
  = bfWalk' [t]
  where
    bfWalk' :: [Tree a] -> [a]
    bfWalk' []
      = []
    bfWalk' (Empty : ts) 
      = bfWalk' ts
    bfWalk' (Node l x r : ts)
      = x : bfWalk' (ts ++ [l, r])

---------------------------------------------------------------------------------

-- This labels a tree breadth-first (from 1) using the given function...

labelWith :: ([Tree a] -> Int -> [Tree Int]) -> Tree a -> Tree Int
labelWith f t 
  = t' 
  where 
    [t'] = f [t] 1

---------------------------------------------------------------------------------

-- labelBF is queue-based and will not work with infinite trees...
-- Of course, reverse could be avoided by using a two-ended list (or queue)

labelBF :: [Tree a] -> Int -> [Tree Int]
labelBF [] n 
  = []

labelBF ts n
  = replaceChildren ts (labelBF allChildren (n + k)) n
  where 
    -- number of labels is not the length of list due to empty trees
    -- count the number of nodes in the forest
    -- hence the isNode is essential
    k = length (filter isNode ts)
    -- concat all children together
    -- concatMap = concat . map 
    allChildren = concatMap children ts
    replaceChildren [] _ _
      = []
    replaceChildren (Empty : ts) cs n
      = Empty : replaceChildren ts cs n
    -- this will not give a non-exhaustive error since there must be at least 2 children for every node. 
    replaceChildren (Node l x r : ts) (l' : r' : cs') n
      = Node l' n r' : replaceChildren ts cs' (n + 1)
      where 

---------------------------------------------------------------------------------
-- The tree of Figure 1 and both its DF and BF relabelling

fig1, fig1RelabelledDF :: Tree Int
fig1 
  = Node (Node (Node Empty 3 (Node Empty 6 Empty)) 7 Empty) 
         8 
         (Node (Node Empty 9 Empty) 11 (Node Empty 15 Empty))

fig1RelabelledDF
  = Node (Node (Node Empty 3 (Node Empty 4 Empty)) 2 Empty) 
         1 
         (Node (Node Empty 6 Empty) 5 (Node Empty 7 Empty))

fig1RelabelledBF
  = Node (Node (Node Empty 4 (Node Empty 7 Empty)) 2 Empty) 
         1 
         (Node (Node Empty 5 Empty) 3 (Node Empty 6 Empty))

-- An infinite tree for testing
tinf :: Tree Int
tinf 
  = Node tinf 0 tinf

