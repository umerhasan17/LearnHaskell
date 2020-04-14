data Instr = Define String 
            | Mov Operand Operand
            | Add Operand Operand
            | Mul Operand Operand

data Operand = Reg Register
                | Ind Register
                | Abs String
                | ImmNum Int
                | ImmName String

data Register = D0 | D1 | D2 | D3 | D4 | D5 | D6 | D7

data Stat = Assign Lhs Exp 
data Lhs = Var String | Array String Exp
data Exp = Ref Lhs | Plus Exp Exp | Num Int

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
    = [Mov (ImmNum Int) (Reg dst)]
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