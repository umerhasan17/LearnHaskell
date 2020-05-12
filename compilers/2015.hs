data Instr =  Define String
            | Mov Operand Operand
            | Cmp Operand Operand
            | Beq String
            | Bne String
            | Bra String
            deriving Show

data Operand = Abs String 
             | ImmNum Int
             deriving Show

data Stat = Assign String Exp | Switch String [(Int, Stat)]
data Exp = Const Int | Var String

transStat :: Stat -> [Instr]
transStat (Assign s (Const n))
    = [Mov (ImmNum n) (Abs s)]
transStat (Assign s (Var s'))
    = [Mov (Abs s') (Abs s)]
transStat (Switch s cases)
    = transCases s cases
    
transCases :: String -> [(Int, Stat)] -> [Instr]
transCases x [(i, s)]
    = [Cmp (Abs x) (ImmNum i), Bne "end"] ++ (transStat s) 
    ++ [Define "end"]
transCases x ((i, s):(i', s'):cases)
    = [Cmp (Abs x) (ImmNum i), Bne next] ++ transStat s 
    ++ [Bra "end", Define next] ++ rest
    where
        next = "case"++(show i')
        rest = transCases x ((i', s'):cases)

-- Current case label -> Cmp x 1 -> Bne next case -> 
    -- current case code -> Bra end -> Next case label -> 

-- example statement
list = [Assign "x" (Const 1), Switch "x" [(1, Assign "y" (Const 10)), (2, Assign "z" (Const 20))]]

-- res = [[Mov (ImmNum 1) (Abs "x")],
-- [Cmp (Abs "x") (ImmNum 1),
-- Bne "case2",
-- Mov (ImmNum 10) (Abs "y"),
-- Bra "end",
-- Define "case2",
-- Cmp (Abs "x") (ImmNum 2),
-- Bne "end",
-- Mov (ImmNum 20) (Abs "z"),
-- Define "end"]]