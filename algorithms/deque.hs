data Deque a = Deque [a] [a]
    deriving (Show)

pushLeft:: a -> Deque a -> Deque a
pushLeft x (Deque xs ys) = Deque (x:xs) ys

pushRight:: a -> Deque a -> Deque a
pushRight y (Deque xs ys) = Deque xs (y:ys)

popLeft:: Deque a -> (a, Deque a)
popLeft d@(Deque [] ys)       = popLeft (reorganise d)
popLeft (Deque (x: xs) ys)  = (x, Deque xs ys)

popRight:: Deque a -> (a, Deque a)
popRight d@(Deque xs [])     = popRight (reorganise d)
popRight (Deque xs (y: ys))  = (y, Deque xs ys)

reorganise:: Deque a -> Deque a
reorganise (Deque [] ys) 
    = (Deque (reverse (drop n ys)) (take n ys))
    where
        n = Prelude.length ys `div` 2
reorganise (Deque xs []) = (Deque (take n xs) (reverse (drop n xs)))
    where
        n = Prelude.length xs `div` 2