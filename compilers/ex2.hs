data Statement = Assign Name Expression | 
                Compound [Statement] |
                IfThen Expression Statement |
                IfThenElse Expression Statement Statement |
                While Expression Statement

data Instruction = 
    Add | Sub | Mul | Div
    | PushImm num
    | PushAbs Name
    | Pop Name
    | CompEq
    | JTrue label
    | JFalse label
    | Jump label
    | Define label

-- we will have to pass off label generation to a new function generateLabel to avoid duplication
translate_statement:: Statement -> [Instruction]
translate_statement (Assign var exp)
    = translate_expression exp ++ [Pop var]
translate_statement (Compound (x : xs))
    = translate_statement x ++ translate_statement (Compound xs)
translate_statement (Compound [])
    = []
translate_statement (IfThen exp s)
    = translate_expression exp ++ [JFalse end] ++ 
    translate_statement s ++ [Define end]
translate_statement (IfThenElse exp sTrue sFalse)
    = translate_expression exp ++ [JFalse false_branch] ++ 
    translate_statement sTrue ++ [Define false_branch] ++
    translate_statement sFalse
translate_statement (While exp s)
    = [Define cond_start] ++ translate_expression exp ++
    [JFalse end] ++ translate_statement s ++ 
    [Jump cond_start, Define end]


translate_expression:: Exp -> [Instr]




--                                 defs       uses       succs preds
data CFGNode = Node Id Instruction [Register] [Register] [Id] [Id]

-- Remember to initialise everything to infinity and the first timeIn to 0. And the start timeOut to 0.
-- Remember to include start node
(Node 0 a=b+c                   [a] [b, c]  [1]     [])
(Node 1 d=a<10                  [d] [a]     [2]     [0])
(Node 2 if d goto L1 else L2    []  [d]     [4, 6]  [1])
(Node 3 b=a-1                   [b] [a]     [4]     [])
(Node 4 a=f(d, e)               [a] [d, e]  [5]     [2])
(Node 5 if a goto L0 else L2    []  [a]     [2, 6]  [4])
(Node 6 a=b                     [a] [b]     [7]     [2, 5])
(Node 7 end                     []  []       []     [6])


Exercise 6:

effect :: PointsToSet → CFGNode → PointsToSet
effect pts (Node id (Cmp r1 r2)) = pts
effect pts (Node id (Bgt label)) = pts
effect pts (Node id (New n r)) = pts ∪ {(r, id)}

effect pts (Node id (New n r))
    | isJust l  = pts / 
    | otherwise = pts ∪ {(r, id)}
    where
        l = lookup r pts

effect pts (Node id (Mov r1 r2)) = 
    | isJust l  = pts u {(r2, fromJust l)}
    | otherwise = pts 

    where
        l = lookup r1 pts

-- {(D0, 1) (D3, 2) (D3, 1)}
-- pointsIn(n)  = pointsOut(n) / effect  instruction at n 

1 Jump 4  {(d0, 1)}
2 Jump 4  {(d0, 1), (d0, 2)}
3 Jump 4  {(d2, 2)}


4 

pointsIn(n)  = for all p E pred(n), {} U pointsOut(p) 
pointsOut(n) = effect pointsIn(n) n 