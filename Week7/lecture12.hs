type Vars = String
-- ^ ::= V| ^ ^ | \ V ^ Grammar of terms
data Lam = Var String | App Lam Lam | Abs Vars Lam

instance Show Lam where
    show (Var x) = x
    show (App s t) = show s ++ "(" ++ show t ++ ")"
    show (Abs x t) = "\\" ++ x ++ "." ++ show t

fv::Lam->[Vars]
fv (Var x) = [x]
fv (App s t) = fv s ++ fv t
fv (Abs x t) = filter (/= x) (fv t) 


subst::(Vars, Lam)->Lam->Lam
subst (x, a) (Var v) = if x == v then a else Var v
subst (x, a) (App b1 b2) = App (subst (x, a) b1) (subst (x, a) b2)
-- subst (x, a) (Abs y b0) = Abs y (subst (x, a) b0)
subst (x, a) (Abs y b0)
    | x == y    = (Abs y b0)
    |not(elem y (fv a)) = Abs y (subst (x, a) b0)
    |otherwise =
        let z = getVar (x:y:fv a ++ fv b0)
            b = subst (y, Var z) b0
            in Abs z (subst (x, a) b)

red::Lam->Lam
red (App (Abs x b) a) = subst (x, a) b
red (Abs x b) = Abs x (red b)
red (App b1 b2) = App (red b1) b2
red (Var x) = Var x

ex::Lam
ex = App(Abs "x" (App(Var "x")(Abs "y" (App(Var "x")(Var "y"))))) (Abs "z" (Var "z"))

-- Haskell is a very lazy language
-- The bottom will run into an overflow
f x = f (x + 1) 

g x = if x < 5 then x else f x -- All parts have to be evaluated
-- But it causes a crash
-- This is an example of call by need.

zeros::[Integer]
zeros = 0 : zeros

-- You can put a function in front of the recursive link
allNums::[Integer]
allNums = 0:map (+1) allNums

fib::[Integer]
fib = 0 : 1 : zipWith (+) fib (tail fib)

primes::[Integer]
primes = sieve [2..] where
    sieve (x:xs) = x : sieve (filter (\y-> mod y x /= 0) xs)

allVars::[String]
allVars = tail vs where
    chars = ['a'..'z']
    vs = "" : concat (map (\v-> map (\c->v++[c]) chars) vs)

getVar::[Vars]->Vars
getVar xs = head(filter (\x -> not (elem x xs)) allVars)
