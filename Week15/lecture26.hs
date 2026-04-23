import Data.Char 

type TVars = String
type Vars  = String
--    T  ::= TV | T -> T 
data Types = TVar TVars | Fun Types Types 
  deriving Eq 
--    /\ ::= V | /\ /\ | \ V /\   -- grammar of terms 
data Lam = Var Vars | App Lam Lam | Abs Vars Lam 

instance Show Types where 
  show (TVar a) = a 
  show (Fun t1 t2) = "(" ++ show t1 ++ " -> " ++ show t2 ++ ")"

instance Show Lam where 
  show (Var x) = x 
  show (App s@(Abs _ _) t) = "(" ++ show s ++ ")" ++ "(" ++ show t ++ ")"
  show (App s (Var x)) = show s ++ x 
  show (App s t) = show s ++ "(" ++ show t ++ ")"
  show (Abs x t) = "\\" ++ x ++ "." ++ show t 

-- functions for dealing with type variables 
tvs :: Types -> [TVars] 
tvs (TVar a) = [a]
tvs (Fun t1 t2) = tvs t1 ++ tvs t2 

tsubst :: (TVars,Types) -> Types -> Types 
tsubst (a,t) (TVar b) = if a==b then t else TVar b 
tsubst (a,t) (Fun t1 t2) = Fun (tsubst (a,t) t1) (tsubst (a,t) t2) 

-- Representation of contexts and type constraints 
type Constr = (Types,Types) 
type Cxt = [(Vars,Types)]

tvsCxt :: Cxt -> [TVars] 
tvsCxt [] = [] 
tvsCxt ((x,t) : gamma) = tvs t ++ tvsCxt gamma 

tsubstConstrList :: (TVars,Types) -> [Constr] -> [Constr] 
tsubstConstrList (a,t) [] = [] 
tsubstConstrList (a,t) ((l,r):cs) = (tsubst (a,t) l , tsubst (a,t) r) : rest 
    where rest = tsubstConstrList (a,t) cs 

-- The core of type inference: generating the constraints 
genConstrs :: Cxt -> Lam -> Types -> [Constr]
genConstrs gamma (Var x) ty = case lookup x gamma of 
      Nothing -> error ("Var not in context: " ++ x) 
      Just  t -> [(t,ty)]
genConstrs gamma (App s t) ty = 
  let a = getVar (tvsCxt gamma ++ tvs ty)
      cs1 = genConstrs gamma s (Fun (TVar a) ty)
      cs2 = genConstrs gamma t (TVar a) 
   in cs1 ++ cs2 
genConstrs gamma (Abs x r) (Fun t1 t2) = 
  genConstrs ((x,t1) : gamma) r t2
genConstrs gamma (Abs x r) ty = 
  let a1 = getVar (tvsCxt gamma ++ tvs ty)
      a2 = getVar (a1 : tvsCxt gamma ++ tvs ty)
      cs = genConstrs ((x,TVar a1) : gamma) r (TVar a2)
   in (ty,Fun (TVar a1) (TVar a2)) : cs 

-- Final stage: Solving the constraints 
type TSub = [(TVars,Types)]

-- Unification: The "solving the constraints" step of Hindley-Milner 
unify :: [Constr] -> TSub 
unify [] = [] 
unify ((lhs,rhs):cs) | lhs == rhs = unify cs 
unify ((TVar a,rhs):cs) | elem a (tvs rhs) = 
  error "Occurs check: Cannot construct infinite type error!"
unify ((TVar a,rhs):cs) = (a,rhs) : unify (tsubstConstrList (a,rhs) cs)
unify ((Fun t1 t2,TVar a):cs) = unify ((TVar a,Fun t1 t2):cs)
unify ((Fun t1 t2 , Fun u1 u2) : cs) = unify ((t1,u1):(t2,u2):cs)
unify _ = error "Type error!"

inferType :: Lam-> Types
inferType t =
    let cs = genConstrs [] t (TVar "a")
        sub = unify cs
    in foldl (flip tsubst) (TVar "a") sub

-- collect all the free variables in a given lambda term 
fv :: Lam -> [Vars]
fv (Var x) = [x]
fv (App s t) = fv s ++ fv t 
fv (Abs x t) = filter (/= x) (fv t)

subst :: (Vars,Lam) -> Lam -> Lam 
subst (x,a) (Var v) = if x==v then a else Var v 
subst (x,a) (App b1 b2) = App (subst (x,a) b1) (subst (x,a) b2) 
subst (x,a) (Abs y b0) 
  | x == y              = (Abs y b0)
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

nf::Lam->Lam
nf t = if t' == t then t else nf t' where t' == 

-- LEXER for lambda terms 

data Token = LPar | RPar | Backslash | Dot | VSym Vars 
           | Err String  | PT Lam | NSym Name | EqSym
  deriving Show 

lexer :: String -> [Token]
lexer "" = []
lexer ('(':s)  = LPar : lexer s 
lexer (')':s)  = RPar : lexer s 
lexer ('\\':s) = Backslash : lexer s 
lexer ('=':s) = EqSym : lexer s
lexer ('.':s)  = Dot : lexer s 
lexer s@(x:xs) | isLower x = VSym var : lexer rest 
  where (var,rest) = span isAlphaNum s
lexer (x:xs) | isSpace x = lexer xs 
lexer s = [Err (take 10 s)]          

-- data Lam = Var Vars | App Lam Lam | Abs Vars Lam 
sr :: [Token] -> [Token] -> [Token]
sr (NSym n:s)q = sr (PT (Const n): s)q
sr (VSym x : s) (Dot : q) = sr (Dot : VSym x : s) q 
sr (VSym x : s) q = sr (PT (Var x) : s) q 
sr (PT t2 : PT t1 : s) q = sr (PT (App t1 t2) : s) q 
sr (PT term : Dot : VSym x : Backslash : s) []  
  = sr (PT (Abs x term) : s) []
sr (PT term : Dot : VSym x : Backslash : s) q@(RPar : _) 
    = sr (PT (Abs x term) : s) q 
sr (RPar : PT term : LPar : s) q = sr (PT term : s) q 
sr (Err s : _) _ = [Err s] 
sr s (q0 : q) = sr (q0 : s) q 
sr s [] = s 

parseConst::[Token]->(Name, Lam)
parseConst (NSym n : EqSym : ts) = case parseTerm ts of
                            Left t -> (n, t)
                            Right e -> error e

parseTerm :: [Token] -> Either Lam String 
parseTerm ts = 
  case sr [] ts of 
    [PT t]  -> Left t 
    [Err t] -> Right ("Lexer error: " ++ t)
    ts      -> Right ("Parser error: " ++ show ts)

ex :: Lam  -- (\x.x(\y.xy))(\z.z)
ex = App (Abs "x" (App (Var "x") (Abs "y" (App (Var "x") (Var "y")))))
         (Abs "z" (Var "z"))

s,k,i,w :: Lam 
s = Abs "x" (Abs "y" (Abs "z" (App (App (Var "x") (Var "z")) (App (Var "y") (Var "z")))))
k = Abs "x" (Abs "y" (Var "x"))
i = Abs "v" (Var "v")
w = Abs "x" (Abs "y" (App (App (Var "x") (Var "y")) (Var "y"))

allVars :: [String] 
allVars = tail vs where 
  chars = ['a'..'z']
  -- vs = "" : concat (map (\v -> map (\c -> v ++ [c]) chars) vs)
  vs = "" : (vs >>= (\v -> map (\c -> v ++ [c]) chars))

getVar :: [Vars] -> Vars 
getVar xs =
  let maxVar = maximum xs 
      nextVar = head (tail (dropWhile (/= maxVar) allVars))
   in nextVar 
-- head (filter (\x -> not (elem x xs)) allVars)

-- (\xy.y(xy))(\z.zz)(\y.yI)
ex2 :: Lam 
ex2 = App (App (Abs "x" (Abs "y" (App (Var "y") (App (Var "x") (Var "y")))))
               (Abs "z" (App (Var "z") (Var "z"))))
          (Abs "y" (App (Var "y") i))

ex3 :: Lam 
ex3 = Abs "x" (Abs "y" (App (Var "y") (Var "x")))
-- the end 

substDefs::[(Name, Lam)] -> Lam->Lam
substDefs defs t = foldl (flip )

main = repl []

repl:: [(Name, Lam)]-> IO()
repl def = do
    inp <- getLine
    case inp of
        ":l" -> do
            putStr "Enter the file to load: "
            filename <- getLine
            filecontents <- readFile filename
            let fileLines = lines filecontents
            let definitions = map (parseConst . lexer) fileLines
            repl definitions
            :"t" -> do
                putStrLn "Enter the term to infer the type for: "
                termstring <- getLine
                case parseTerm (lexer termstring) of
                    Right err-> putStrLn err >> repl def
                    Left term -> do
                        let t = infertype (substDefs def term)
                        putStrLn (show t)
                        repl def
