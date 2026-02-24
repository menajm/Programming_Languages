type Vars = String
-- ^ ::= V| ^ ^ | \ V ^ Grammar of terms
data Lam = Var String | App Lam Lam | Abs Vars Lam

instance Show Lam where
    show (Var x) = x
    show (App s t) = show s ++ "(" ++ show t ++ ")"
    show (Abs x t) = "\\" ++ x ++ "." ++ show t

subst::(Vars, Lam)->Lam->Lam
subst (x, a) (Var v) = if x == v then a else Var v
subst (x, a) (App b1 b2) = App (subst (x, a) b1) (subst (x, a) b2)
subst (x, a) (Abs y b0) = Abs y (subst (x, a) b0)

red::Lam->Lam
red (App (Abs x b) a) = subst (x, a) b
red (Abs x b) = Abs x (red b)
red (App b1 b2) = App (red b1) b2
red (Var x) = Var x

ex::Lam
ex = App(Abs "x" (App(Var "x")(Abs "y" (App(Var "x")(Var "y"))))) (Abs "z" (Var "z"))

