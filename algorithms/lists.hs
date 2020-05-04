{-# LANGUAGE InstanceSigs #-}

class List list where
    fromList:: [a] -> list a

    toList:: list a -> [a]

    normalize:: list a -> list a
    normalize = fromList.toList

    empty:: list a
    empty = fromList []

    single:: a -> list a
    single a = fromList [a]

    cons:: a -> list a -> list a
    cons a xs
        = fromList (a : toList xs)

    snoc:: a -> list a -> list a
    snoc a xs
        = fromList (toList xs Prelude.++ [a])

    head:: list a -> a
    head = Prelude.head.toList

    tail:: list a -> list a
    tail = fromList.Prelude.tail.toList

    isEmpty:: list a -> Bool
    isEmpty = null.toList

    isSingle:: list a -> Bool
    isSingle xs
        = Prelude.length (toList xs) == 1

    length:: list a -> Int
    length = Prelude.length.toList

    (++):: list a -> list a -> list a
    xs ++ ys = fromList ((toList xs) Prelude.++ toList(ys))

instance List [] where
    fromList = id
    toList = id


-- Difference lists

newtype DList a = DList ([a] -> [a])

instance List DList where
    fromList:: [a] -> DList a
    fromList xs = DList (xs Prelude.++)
    toList:: DList a -> [a]
    toList (DList fxs) = fxs []
    (++):: DList a -> DList a -> DList a
    (DList fxs) ++ (DList fys) = DList (fxs . fys)


-- Random Access Lists

data Tree a = Leaf a | Node (Tree a) a (Tree a)

ttList:: Tree a -> [a]
ttList t = ttList' t []
    where
        ttList' :: Tree a -> [a] -> [a]
        ttList' (Leaf x) xs = (x: xs)
        ttList' (Node t1 x t2) xs = ttList' t1 (x : ttList' t2 xs)

insertT:: a -> Tree a -> Tree a
insertT elem t
    = Leaf elem

newtype RAList a = RAList [Tree a]

instance List RAList where
    toList (RAList ts) = (concat . map ttList) ts

    fromList xs = foldr insert (RAList []) xs
        where
            insert :: a -> RAList a -> RAList a
            insert elem ral
                = insert' elem ral 1
            insert' :: a -> RAList a -> Int -> RAList a
            -- empty place so create a new tree
            insert' elem (RAList []) size
                = RAList [Leaf elem]
            -- if tree not full then insert in tree, otherwise keep going
            insert' elem (RAList (x:xs)) size
                | Prelude.length (ttList x) < size =  (RAList ((insertT elem x) : xs))
                | otherwise                        =  insert' elem (RAList xs) (size*2)  


data TreeTip a = Tip | Leaf a | Fork (TreeTip a) (TreeTip a)

instance List TreeTip where
    fromList:: [a] -> TreeTip a
    fromList [] = Tip
    fromList (x:xs) = Fork (Leaf x) (fromList xs)

    toList:: TreeTip a -> [a]
    toList Tip = []
    toList Leaf x = [x]
    toList Fork x y = toList x ++ toList y
