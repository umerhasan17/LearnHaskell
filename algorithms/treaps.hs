import StdGen

-- randomized algorithm (monte carlo fails some of the time or las vegas takes a long time sometimes)

-- in a treap all priorities should be unique

data Treap a = Empty | Node (Treap a) a Int (Treap a)
    deriving (Show)

member:: Ord a => a -> (Treap a) -> Bool
member x Empty
    = False
member x (Node l v _ r)
    | x == v = True
    | x < v  = member x l
    | otherwise = member x r

-- lnode and rnode make sure the tree is balanced on that side
insert:: Ord a => a -> Int -> (Treap a) -> (Treap a)
insert x p Empty
    = Node Empty x p Empty
insert x p (Node l v q r)
    | x < v  = lnode (insert x p l) v q r
    | x == v = Node l v p r
    | x > v  = rnode l v q (insert x p r)

-- comparing priorities here only, furthermore Ord a is assumed by insert calls
lnode:: Treap a -> a -> Int -> Treap a -> Treap a
lnode Empty v q r = Node Empty v q r
lnode l@(Node l' x p r) v q r'
    | q <= p    = Node l v q r'
    | otherwise = Node l' x p (Node r v q r')

rnode:: Treap a -> a -> Int -> Treap a -> Treap a
rnode l v q Empty = Node l v q Empty
rnode l x p r@(Node l' v q r')
    | p <= q    = Node l x p r
    | otherwise = Node (Node l x p l') v q r'

delete:: Ord a => a -> Treap a -> Treap a
delete x Empty = Empty
delete x t@(Node a y p b)
    -- REMEMBER TO BUILD THE TREE BACK UP USING NODE
    | x < y  = Node (delete x a) y p b
    | x == y = merge a b
    | x > y  = Node a y p (delete x b)

-- how does this work?
merge:: Treap a -> Treap a -> Treap a
merge Empty x = x
merge x Empty = x
merge l@(Node a x p b) r@(Node c y q d)
    | p <= q    = Node a x p (merge b r)
    | otherwise = Node (merge l c) y q d

toList:: Treap a -> [(a, Int)] -> [(a, Int)]
toList Empty l = l
toList (Node a x p b) l
  = toList a ((x, p) : toList b l)

fromList:: Ord a => [(a, Int)] -> Treap a
fromList [] = Empty
-- fromList ((x, p) : xs) = insert x p (fromList xs)
-- from the notes
fromList xs = foldr (uncurry insert) Empty xs


-- randomised quicksort
-- feed in randomness to maintain Leibniz Law i.e. x = y ==> f x = f y
rquicksort:: Ord a => StdGen -> [a] -> [a]
rquicksort g xs = toList (fromList (zip xs ps))
    where
        ps = randoms g


-- https://ux.stackexchange.com/questions/73445/is-a-higher-priority-a-smaller-number

-- randomised BSTs will insert an element properly but with probability 1/(n+1) will insert at the root.