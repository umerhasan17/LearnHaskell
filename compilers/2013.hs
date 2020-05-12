{-|

if there is invariant code insert into loop pre-header



-}

data Instr = Define String 
            | Mov Operand Operand
            | Sub Operand Operand
            | Cmp Operand Operand
            | Blt String
            | Bra String

data Operand = Reg Register
                | Abs String
                | ImmNum Int

data Register = D0 | D1 | D2 | D3 | D4 | D5 | D6 | D7

data Stat = Assign String Exp | DoWhile [Stat] BExp | Break
data Exp = Const Int | Var String | Minus Exp Exp 
data BEXp = LessThan Exp Exp 

transStat :: Stat -> [Register] -> [Instr]
transStat (Assign s e) r@(dst:next:rest)
    = (transExp e r) ++ [Mov (Reg dst) (Abs s)]
transStat (DoWhile xs b) r
    = (Define s) : (transWhile xs e r) ++ (transBexp b s r) ++ (Define e)
    -- = (Define s) : (map (transStat xs r)) ++ (transBexp b s r)
    where
        s = "startLabel"
        e = "endLabel"
transStat (Break) r = undefined

transWhile:: [Stat] -> String -> [Register] -> [Instr]
transWhile [] _ _
    = []
transWhile (x: xs) end r
    = (transStat x r) ++ (transWhile xs end r)
transWhile (Break : xs) end r
    = (Bra end) : (transWhile xs r)


transBexp :: BEXp -> String -> [Register] -> [Instr]
transBexp (LessThan e1 e2) r@(dst:next:rest) label
    = (transExp e1 r) ++ (transExp e2 (next:rest)) ++ 
    [Cmp (Reg dst) (Reg next)] ++ [Blt label]



transExp:: Exp -> [Register] -> [Instr]
-- don't do this
-- transExp (Plus e1 e2) r@(dst:next:rest)
--     = (transExp e1 r) ++ [Mov dst next] ++ 
--     (transExp e2 (dst:rest)) ++ [Add (next) (dst)]
transExp (Plus e1 e2) r@(dst:next:rest)
    = (transExp e1 r) ++ 
    (transExp e2 (next:rest)) ++ 
    [Add (Reg next) (Reg dst)]
transExp (Num a) (dst:rest) 
    = [Mov (ImmNum a) (Reg dst)]
transExp (Ref lhs) r
    = transLhs lhs r

transLhs:: Lhs -> [Register] -> [Instr]
transLhs (Var str) (dst:rest)
    = [Mov (Abs str) (Reg dst)]
transLhs (Array str exp) r@(dst:rest)
    = (transExp exp r) ++ [Mul (ImmNum 4) (Reg dst)] ++ 
    [Add (Abs str) (Reg dst)] ++ [Mov (Ind dst) (Reg dst)]

transStat:: Stat -> [Register] -> [Instr]
transStat (Assign l e) r@(dst:next:rest)
    = (transLhs l r) ++ (transExp e (next:rest)) ++ 
    [Mov (Reg next) (Reg dst)] 