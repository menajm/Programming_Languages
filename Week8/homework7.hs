
import Data.List

-- ----Section 2 Extending the syntax of boolean formulas ----
type Vars = String
data Prop = Var Vars | Const Bool | And Prop Prop | Or Prop Prop | Not Prop
    | Imp Prop Prop | Iff Prop Prop | Xor Prop Prop
    deriving (Show,Eq)

prop1 = Var "X" `And` Var "Y"
prop2 = Var "X" `Imp` Var "Y"
prop3 = Not (Var "X") `Or` (Var "Y")
prop4 = Not (Var "X") `Iff` Not (Var "Y")

-- 2.1 Variables
fv :: Prop-> [Vars]
fv (Var v) = [v]
fv (Const _) = []
fv (And p q) = nub(fv p ++ fv q)
fv (Or p q) = nub(fv p ++ fv q)
fv (Not p) = fv p
fv (Imp p q) = nub(fv p ++ fv q)
fv (Iff p q) = nub(fv p ++ fv q)
fv (Xor p q) = nub(fv p ++ fv q)

-- 2.2 Evaluator
type Value = Bool
type Env = [(Vars, Value)]
eval::Env->Prop->Value
eval env (Var v) = case lookup v env of
        Just val -> val
        Nothing -> error "No variable"
eval env (Const c) = c
eval env (Not p) = not (eval env p)
eval env (And p q) = eval env p && eval env q
eval env (Or p q) = eval env p || eval env q
eval env (Imp p q) = not (eval env p) || eval env q
eval env (Iff p q) = eval env p == eval env q
eval env (Xor p q) = eval env p /= eval env q

-- ---Section 3 Classification of Formulas---
