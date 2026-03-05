type Vars = String
type Value = Integer
type Env = [(Vars, Value)]

data AExpr = Var Vars | Num Integer | Add AExpr AExpr | Sub AExpr AExpr
                                    | Mul AExpr AExpr | Div AExpr AExpr | Exp AExpr AExpr
    deriving Show
data BOps = AddOp | SubOp | MulOp | DivOp | ExpOp
    deriving Show
data Token = VSym String | CSym Integer | LPar | RPar | BOp BOps | EqSym | Err String
                | PA AExpr
    deriving Show

-- For every grammar rule the parser checks if it can be reduced
parseAExpr::[Token]->AExpr
parseAExpr ts = case sr [] ts of
    [PA e] -> e
    [Err e] -> error ("LLexical error:" ++ e)
    st -> error("Parse error:" ++ show st)

rank::BOps->Integer
rank AddOp = 10
rank SubOp = 10
rank MulOp = 20
rank DivOp = 20

-- sr is a shift-reduce helper
-- sr queue outputs the final state of the stack
sr::[Token]->[Token]->[Token]
sr (Err e:s) q = [Err e]
-- Reduce phase
-- One clause per grammar rule!
sr (VSym x : s) q = sr (PA(Var x):s)q
sr (CSym x : s) q = sr (PA(Num x):s)q
-- Fix the PEMDAS issue by doing this. Before, there was no way to rank the sub/add mul/div
sr stack@(PA e2 : BOp op : PA e1 : s)(BOp op2 : q) 
    | rank op < rank op2 = sr (BOp op2 : stack) q
sr (PA e2 : BOp AddOp : PA e1 : s) q = sr (PA (Add e1 e2) : s)q
sr (PA e2 : BOp SubOp : PA e1 : s) q = sr (PA (Sub e1 e2) : s)q
sr (PA e2 : BOp MulOp : PA e1 : s) q = sr (PA (Mul e1 e2) : s)q
sr (PA e2 : BOp DivOp : PA e1 : s) q = sr (PA (Div e1 e2) : s)q
sr (RPar : PA e : LPar : s) q = sr (PA e : s) q
-- Shift phase (needs to come after)
sr s (q:qs) = sr (q:s) qs
sr [PA e] [] = e
sr s [] = error "No parse!"

ex1, ex2::String
ex1 = "3*x + y"
ex2 = "(2*17-5*4)/7"

out1, out2::AExpr
out1 = Add(Mul(Num 3)(Var "x"))(Var "y")
out2 = Div(Sub (Mul (Num 2)(Num 17)) (Mul(Num 5) (Num 4))) (Num 7)


lexer::String-> [Token]
lexer "" = []
lexer (x:xs) | isSpace x = lexer xs
-- Constants
lexer (x:xs) | isDigit x = CSym (read n) : lexer ys
    where (n, ys) = span isDigit (x:xs)
-- Variables
lexer (x:xs) | isLower x = VSym v : lexer ys
    where (v, ys) = span isAlphaNum (x:xs)
-- Operators and parens
lexer ('+':xs)= BOp AddOp : lexer xs
lexer ('-':xs)= BOp SubOp : lexer xs
lexer ('*':xs)= BOp MulOp : lexer xs
lexer ('/':xs)= BOp DivOp : lexer xs
lexer ('^':xs)= BOp ExpOp : lexer xs

lexer ('(':xs)= LPar : lexer xs
lexer (')':xs)= RPar : lexer xs
lexer ('=':xs)= EqSym : lexer xs
lexer xs = [Err (drop 10 xs)]

eval:: Env->AExpr->Value
eval env (Var v) = case lookUp v env of
        Just x -> x 
        Nothing -> error "Variable not found!"
eval env (Num c) = c
eval env (Mul e1 e2) = eval env e1 * eval env e2
eval env (Add e1 e2) = eval env e1 + eval env e2
eval env (Sub e1 e2) = eval env e1 - eval env e2
eval env (Div e1 e2) = eval env e1 `div` eval env e2