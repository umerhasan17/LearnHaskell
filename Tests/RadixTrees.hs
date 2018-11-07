data IntTree = EmptyTree | InternalNode IntTree Int IntTree
               deriving Show

buildIntTree :: [Int] -> IntTree
buildIntTree = foldr add EmptyTree

add x EmptyTree = InternalNode EmptyTree x EmptyTree
add x ( InternalNode l y r ) 
  = if x == y 
    then ( InternalNode l y r )
    else if x <= y then InternalNode ( add x l ) y r
                   else InternalNode l y ( add x r )

-- Simple random number generator...
a, m :: Integer
m = 1073741824
a = 16387

rand s 
  = fromInteger s / fromInteger m : rand s' where s' = ( s * a ) `mod` m
          
randomInts :: Int -> Int -> Integer -> [ Int ]
randomInts m n s 
  = take m ( map ( round . (+1) . (* (fromIntegral n) ) ) ( rand s ) )

rs :: [ Int ]
rs = randomInts 1000 1000 816211275

rs1 :: [ Int ]
rs1 = randomInts 20 1000 816211275

--------------------------------------------------------------------------

data Colour = Black | White
              deriving ( Eq, Ord, Show )

data RadixTree = Leaf Colour | Node Colour RadixTree RadixTree 
                 deriving ( Eq, Ord, Show )

type BitString = [ Int ]


-----------------------------------------------------------------------------
-- Some test trees...

figure :: RadixTree
figure = Node Black ( Leaf White )
                    ( Node White ( Leaf Black )
                                 ( Node White ( Node Black ( Leaf White )
                                                           ( Leaf Black ) )
                                              ( Leaf White ) ) )

t1 :: IntTree
t1 = InternalNode ( InternalNode EmptyTree 
                                 8 
                                 ( InternalNode EmptyTree 12 EmptyTree ) )
                  20
                  EmptyTree

t2 :: RadixTree
t2 = Node Black ( Node Black ( Leaf White ) 
                             ( Node White ( Leaf Black ) ( Leaf White ) ) ) 
                ( Leaf White )

--------------------------------------------------------------------------

size :: IntTree -> Int
size EmptyTree
  = 1
size (InternalNode t1 _ t2)
  = 13 + size t1 + size t2

size' :: RadixTree -> Int 
size' (Leaf _) 
  = 1
size' (Node _ t1 t2)
  = 9 + size' t1 + size' t2

empty :: RadixTree
empty
  = Leaf Black

binary :: Int -> BitString 
binary n
  | n < 2     = [n]
  | otherwise = binary (n `div` 2) ++ [n `mod` 2]

insert :: BitString -> RadixTree -> RadixTree
insert [] (Leaf _)
  = Leaf White
insert [] (Node _ t t')
  = Node White t t'
insert bs (Leaf c)
  = insert bs (Node c empty empty)
insert (b : bs) (Node c t t')
  | b == 0    = Node c (insert bs t) t'
  | otherwise = Node c t (insert bs t')

buildRadixTree :: [ Int ] -> RadixTree
buildRadixTree []
  = empty
buildRadixTree (n : ns)
  = insert (binary n) (buildRadixTree ns)

-- same function by writing foldr (insert . binary) empty ns
-- binary on n followed by insert. base case is empty. foldr does it on every n.

member :: Int -> RadixTree -> Bool
member
  = undefined

union, intersection :: RadixTree -> RadixTree -> RadixTree
union
  = undefined

intersection
  = undefined
