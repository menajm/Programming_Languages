import Data.Char

type TVars = String
type Vars = String 
-- T ::= TV | T -> T
data Types = TVar TVars | Fun Types Types
--    /\ ::= V | /\ /\ | \ V /\   -- grammar of terms 
data Lam = Var Vars | App Lam Lam | Abs Vars Lam 

instance Show Types where
    show (TVar a) = a


instance Show Lam where 
  show (Var x) = x 
  show (App s@(Abs _ _) t) = "(" ++ show s ++ ")" ++ "(" ++ show t ++ ")"
  show (App s (Var x)) = show s ++ x
  show (App s t) = show s ++ "(" ++ show t ++ ")"
  show (Abs x t) = "\\" ++ x ++ "." ++ show t 

-- Functions for dealing with type variables
tvs::Types->[TVars]
tvs (TVar a) = [a]
tvs (Fun t1 t2) = tvs t1 ++ tvs t2

tsubst:: (TVars, Types)-> Types -> Types
tsubst (a, t) (TVar b) 
    | a == b    = t
    | otherwise = TVar b
tsubst (a, t) (Fun t1 t2) = Fun (tsubst (a, t) t1) (tsubst (a, t) t2)

-- Representation of contexts and type constraints
type Constr = (Types, Types)
type Cxt = [(Vars, Types)]

tvsCxt :: Cxt -> [TVars]
tvsCxt [] = []
tvsCxt ((x, t): gamma) = tvs t ++ tvsCxt gamma

tsubstConstrList :: (TVars, Types) -> [Constr] -> [Constr]
tsubstConstrList (a, t) [] = []
tsubstConstrList (a, t) ((l, r):cs) = (tsubst (a, t) l, tsubst (a, t) r) : rest
    where rest = tsubstConstrList (a, t) cs

-- The core of type inference: generating the constraints
genConstrs:: Cxt->Lam->Types-> [Constr]
genConstrs gamma (Var x) ty = case lookup x  gamma of
                        Nothing -> error ("Var not in context: " ++ x)
                        Just t -> [(t, ty)] 
genConstrs gamma (App s t) ty =
                        let a = getVar(tvsCxt gamma ++ tvs ty)
                            cs1 = genConstrs gamma s (Fun (TVar a) ty)
                            cs2 = genConstrs gamma t (TVar a)
                        in cs1 ++ cs2
genConstrs gamma (Abs x r) (Fun type1 type2) =
                        genConstrs ((x, type1): gamma) r type2
genConstrs gamma (Abs x r) ty = 
                        let a1 = getVar (tvsCxt gamma ++  tvs ty)
                            a2 = getVar (a1 : tvsCxt gamma ++ tvs ty)
                            cs = genConstrs ((x, TVar a1): gamma) r (TVar a2)
                        in (ty, Fun (TVar a1) (TVar a2)) : cs


-- Final stage is solving the constraints
type TSub = [(TVars, Types)]

-- Solving the constraints step
unify::[Constr]->TSub
unify [] = []
unify ((lhs, rhs):cs) | lhs == rhs = unify cs
unify ((TVar a, rhs):cs) | elem a (tvs rhs) = error "Occurs check: Cannot construct infinite type error!" 
unify ((TVar a, rhs):cs) = (a, rhs) : unify (tsubstConstrList (a, rhs) cs)
unify ((Fun t1 t2, TVar a):cs) = unify ((TVar a), Fun t1 t2):cs
unify ((Fun t1, t2, Fun v1 v2):cs) = unify ((t1, v1):(t2, v2):cs)
unify _ = errpr "Type error!"

-- Collect all the free variables in a given lamda term
fv :: Lam -> [Vars]
fv (Var x) = [x]
fv (App s t) = fv s ++ fv t 
fv (Abs x t) = filter (/= x) (fv t)

subst :: (Vars,Lam) -> Lam -> Lam 
subst (x,a) (Var v) = if x==v then a else Var v 
subst (x,a) (App b1 b2) = App (subst (x,a) b1) (subst (x,a) b2) 
-- subst (x,a) (Abs y b0) = Abs y (subst (x,a) b0) 
subst (x,a) (Abs y b0) 
  | x == y     = (Abs y b0)
  | not (elem y (fv a)) = Abs y (subst (x,a) b0) 
  | otherwise = 
    let z = getVar (x:y:fv a ++ fv b0)
        b = subst (y,Var z) b0 
     in Abs z (subst (x,a) b)


red :: Lam -> Lam 
red (App (Abs x b) a) = subst (x,a) b 
red (Abs x b) = Abs x (red b) 
red (App b1 b2) = App (red b1) (red b2) 
red (Var x) = Var x 

-- Lexer for lambda
data Token = LPar | RPar | Backslash | Dot | VSym  Vars
                | Err String | PT Lam | NSym String

lexer:: String->[Token]
lexer "" = [] -- Base case
lexer ('(':s) = LPar : lexer s
lexer (')':s) = RPar : lexer s

lexer ('\\':s) = Backslash : lexer s
lexer ('.':s) = Dot : lexer s

lexer s@(x:_) | isLower x = VSym var : lexer rest
    where (var, rest) = span isAlphaNum s

lexer (x:xs) | isSpace x = lexer xs

lexer s = [Err (take 10 s)]


parseTerm :: [Term]-> Either Lam String
parseTerm ts = case sr[] ts of
                [PT t] -> Left t
                [Err t] -> Right ("Lexer Error: " ++ t)
                ts -> Right ("Parse error: "++ show ts)

-- sr relies on: data Lam = Var Vars | App Lam Lam | Abs Vars Lam 
sr::[Token]->[Token]->[Token]
sr (VSym x : s) (Dot : q) = sr (Dot : VSym s : s) q
sr (VSym x : s)q = sr (PT (Var x): s) q
sr (PT t2 : PI t1 : s)q = sr(PT(App t1 t2):s)q
sr (PT term : Dot : VSym x : Backslash : s)q = sr (PT (Abs x term):s)q


ex :: Lam  -- (\x.x(\y.xy))(\z.z)
ex = App (Abs "x" (App (Var "x") (Abs "y" (App (Var "x") (Var "y")))))
         (Abs "z" (Var "z"))

s,k,i,w :: Lam 
s = Abs "x" (Abs "y" (Abs "z" (App (App (Var "x") (Var "z")) (App (Var "y") (Var "z")))))
k = Abs "x" (Abs "y" (Var "x"))
i = Abs "v" (Var "v")
w = Abs "x" (Abs "y" (App (App (Var "x") (Var "y")) (Var "y")))

allVars :: [String] 
allVars = tail vs where 
  chars = ['a'..'z']
  -- vs = "" : concat (map (\v -> map (\c -> v ++ [c]) chars) vs)
  vs = "" : (vs >>= (\v -> map (\c -> v ++ [c]) chars))

getVar :: [Vars] -> Vars 
getVar xs = head (filter (\x -> not (elem x xs)) allVars)

-- (\xy.y(xy))(\z.zz)(\y.yI)
ex2 :: Lam 
ex2 = App (App (Abs "x" (Abs "y" (App (Var "y") (App (Var "x") (Var "y")))))
               (Abs "z" (App (Var "z") (Var "z"))))
          (Abs "y" (App (Var "y") i))


-- the end 
