import Data.List

type Index = Int

data BExp = Prim Bool | IdRef Index | Not BExp | And BExp BExp | Or BExp BExp
            deriving (Eq, Ord, Show)

type Env = [(Index, Bool)]

type NodeId = Int

type BDDNode =  (NodeId, (Index, NodeId, NodeId))

type BDD = (NodeId, [BDDNode])

------------------------------------------------------
-- PART I

-- Pre: The item is in the given table
lookUp :: Eq a => a -> [(a, b)] -> b
lookUp k t
  = [value | (key, value) <- t, k == key] !! 0

checkSat :: BDD -> Env -> Bool
checkSat (id, ns) e
  | id == 0    = False
  | id == 1    = True
  | value      = checkSat (r, ns) e
  | otherwise  = checkSat (l, ns) e
  where
    (i, l, r) = lookUp id ns
    value     = lookUp i e

sat :: BDD -> [[(Index, Bool)]]
sat b
  = undefined
  -- = sat' b
  -- where
  --   sat' :: BDD -> [[(Index, Bool)]]
  --   sat' (id, ns)
  --     | i == 0 = []
  --     | i == 1 = [[]]
  --     | 
  --     | 
  --   where
  --     (i, l, r) = lookUp id ns

------------------------------------------------------
-- PART II

simplify :: BExp -> BExp
simplify (Not (Prim b))
  = (Prim (not b))
simplify (And (Prim b) (Prim b'))
  = (Prim (b && b'))
simplify (Or (Prim b) (Prim b'))
  = (Prim (b || b'))
simplify e
  = e

-- replace the index with the boolean 
-- simplify 
-- restrict again 

restrict :: BExp -> Index -> Bool -> BExp
restrict (IdRef i) index b
  | i == index = (Prim b)
  | otherwise  = (IdRef i)
restrict (Not e) index b
  = simplify (Not (restrict e index b))
restrict (And e e') index b
  = simplify (And (restrict e index b) (restrict e' index b))
restrict (Or e e') index b
  = simplify (Or (restrict e index b) (restrict e' index b))
restrict e _ _
  = e

------------------------------------------------------
-- PART III

-- Pre: Each variable index in the BExp appears exactly once
--     in the Index list; there are no other elements
-- The question suggests the following definition (in terms of buildBDD')
-- but you are free to implement the function differently if you wish.
buildBDD :: BExp -> [Index] -> BDD
buildBDD 
  =  buildBDD’ e 2 xs

-- Potential helper function for buildBDD which you are free
-- to define/modify/ignore/delete/embed as you see fit.
buildBDD' :: BExp -> NodeId -> [Index] -> BDD
buildBDD' e i (x : y : ys)
  | x > y     = buildBDD' e (2 * i + 1) (y : ys)
  | otherwise = (id2, ()  : nodes2)
  where
    (id1, nodes1) = buildBDD' e (2 * i + 1) (y : ys)
    (id2, nodes2) = buildBDD' e (2 * i) (y : ys)
buildBDD' e i (x : xs)
  = 
buildBDD' e i []
  | e' == (Prim True) = (1, [])
  | otherwise         = (0, [])
  where
    e' = restrict e i 


-- 
-- 
------------------------------------------------------
-- PART IV

-- Pre: Each variable index in the BExp appears exactly once
--      in the Index list; there are no other elements
buildROBDD :: BExp -> [Index] -> BDD
buildROBDD 
  = undefined

------------------------------------------------------
-- Examples for testing...

b1, b2, b3, b4, b5, b6, b7, b8 :: BExp
b1 = Prim False
b2 = Not (And (IdRef 1) (Or (Prim False) (IdRef 2)))
b3 = And (IdRef 1) (Prim True)
b4 = And (IdRef 7) (Or (IdRef 2) (Not (IdRef 3)))
b5 = Not (And (IdRef 7) (Or (IdRef 2) (Not (IdRef 3))))
b6 = Or (And (IdRef 1) (IdRef 2)) (And (IdRef 3) (IdRef 4))
b7 = Or (Not (IdRef 3)) (Or (IdRef 2) (Not (IdRef 9)))
b8 = Or (IdRef 1) (Not (IdRef 1))

bdd1, bdd2, bdd3, bdd4, bdd5, bdd6, bdd7, bdd8 :: BDD
bdd1 = (0,[])
bdd2 = (2,[(4,(2,1,1)),(5,(2,1,0)),(2,(1,4,5))])
bdd3 = (5,[(5,(1,0,1))])
bdd4 = (2,[(2,(2,4,5)),(4,(3,8,9)),(8,(7,0,1)),(9,(7,0,0)),
           (5,(3,10,11)),(10,(7,0,1)),(11,(7,0,1))])
bdd5 = (3,[(4,(3,8,9)),(3,(2,4,5)),(8,(7,1,0)),(9,(7,1,1)),
           (5,(3,10,11)),(10,(7,1,0)),(11,(7,1,0))])
bdd6 = (2,[(2,(1,4,5)),(4,(2,8,9)),(8,(3,16,17)),(16,(4,0,0)),
           (17,(4,0,1)),(9,(3,18,19)),(18,(4,0,0)),(19,(4,0,1)),
           (5,(2,10,11)),(10,(3,20,21)),(20,(4,0,0)),(21,(4,0,1)),
           (11,(3,22,23)),(22,(4,1,1)),(23,(4,1,1))])
bdd7 = (6,[(6,(2,4,5)),(4,(3,8,9)),(8,(9,1,1)),(9,(9,1,0)),
           (5,(3,10,11)),(10,(9,1,1)),(11,(9,1,1))])
bdd8 = (2,[(2,(1,1,1))])

