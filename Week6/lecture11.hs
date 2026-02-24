type Vars = String
type Value = Integer
type Env = [(Vars, Value)]


data AExpr = Var Vars | IConst Value
        |Mul AExpr AExpr | Add AExpr AExpr 
        | Sub AExpr AExpr | Div AExpr AExpr

ex::AExpr
ex = Add (Mul (IConst 2)(Var "x")) (Sub (IConst 3) (Var "x"))

countOps::AExpr->Integer
countOps (Var v) = 0
countOps (IConst c) = 0
countOps (Mul e1 e2) = countOps e1 + countOps e2 + 1
countOps (Add e1 e2) = countOps e1 + countOps e2 + 1
countOps (Sub e1 e2) = countOps e1 + countOps e2 + 1
countOps (Div e1 e2) = countOps e1 + countOps e2 + 1

ex2::AExpr
ex2 = Sub(Div (IConst 12)(IConst 2))
            (Mul (IConst 5) (Add (IConst 3) (IConst 1)))

lookUp::Vars->Env->Value
lookUp v [] = error ("Variable not found in environment" ++ v)
lookUp v (x:xs) = if v == fst x then snd x else lookUp v xs
-- or you can use v ((var, val)xs)if v == var then val else lookUp v xs

eval:: Env->AExpr->Value
eval env (Var v) = lookUp v env
eval env (IConst c) = c
eval env (Mul e1 e2) = eval env e1 * eval env e2
eval env (Add e1 e2) = eval env e1 + eval env e2
eval env (Sub e1 e2) = eval env e1 - eval env e2
eval env (Div e1 e2) = eval env e1 `div` eval env e2

ex3::AExpr
ex3 = Sub(Mul (IConst 2) (Mul (Var "x") (Var "y")))
        (Mul (Var "y") (Var "y"))

-- You can also use lookup which is the safer version than the function we used earlier
-- eval env (Var v) = case lookup v env of
    -- Just x -> x
    -- Nothing -> error "Variable not found!"

getVars::AExpr->[Vars]
getVars (Var x) = [x]
getVars (IConst n) = []
getVars (Add e1 e2) = getVars e1 ++ getVars e2
getVars (Mul e1 e2) = getVars e1 ++ getVars e2
getVars (Sub e1 e2) = getVars e1 ++ getVars e2
getVars (Div e1 e2) = getVars e1 ++ getVars e2