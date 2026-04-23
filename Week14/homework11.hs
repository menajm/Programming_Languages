import Data.Char

type Vars = String
type TVars = String

data Types = Ints | Fun Types Types | TVar TVars
    deriving (Show, Eq)

data Terms = Var Vars | App Terms Terms | Abs Vars Terms
                | Num Integer | Sub Terms Terms 
                | IfPos Terms Terms Terms | Y
    deriving (Show, Eq)

data Token = VSym String | CSym Integer | SubOp
                | IfPositiveK | ThenK | ElseK | YComb
                | LPar | RPar | Dot | Backslash | Err String
                | PT Terms
    deriving (Show, Eq)



-- ---Section 2 Parsing and Lexical Analysis---

lexer :: String -> [Token]
lexer "" = []
lexer (x:xs)
    | isSpace x = lexer xs
    | isLower x = 
        let (var, rest) = span isAlphaNum (x:xs) 
            in case var of
                "ifPositive" -> IfPositiveK : lexer rest
                "then" -> ThenK : lexer rest
                "else" -> ElseK : lexer rest
                _ -> VSym var : lexer rest
    | isDigit x = 
        let (num, rest) = span isDigit (x:xs)
            in CSym (read num): lexer rest
    | x == '-' = SubOp : lexer xs
    | x == 'Y' = YComb : lexer xs
    | x == '(' = LPar : lexer xs
    | x == ')' = RPar : lexer xs 
    | x == '.' = Dot : lexer xs
    | x == '\\' = Backslash : lexer xs
    | otherwise = [Err (take 10 (x:xs))]

parser :: [Token] -> Either Terms String
parser = sr []

-- Helper for parser
sr :: [Token] -> [Token] -> Either Terms String
sr [PT t] [] = Left t
-- Error cases
sr stack [] = Right ("Parse Error: " ++  show stack)
sr _ (Err e : _) = Right ("Lexical Error: " ++ e)

-- Reduction rules
sr (RPar : PT m : LPar : s) q = sr (PT m : s) q

sr (PT u : ElseK : PT t : ThenK : PT s : IfPositiveK : s') q = 
    sr (PT (IfPos s t u): s')q

sr (PT t : SubOp : PT s : s') q = sr (PT (Sub s t): s') q

sr (PT m : Dot : VSym x : Backslash : s) q = sr (PT (Abs x m): s) q

sr (PT n : PT m : s) q
    | null q || isNotOperator (head q) = sr (PT (App m n): s) q
        where isNotOperator t = t `notElem` [Dot, SubOp, ThenK, ElseK]

sr (VSym x : s)(Dot : q) = sr (Dot : VSym x : s) q
sr (VSym x : s)q = sr (PT (Var x) : s)q
sr (CSym c : s)q = sr (PT (Num c) : s) q
sr (YComb : s)q = sr (PT Y : s) q

sr s (t:q) = sr (t:s) q


-- --- Section 3 Typing ---
type Constr = (Types,Types)
type Cxt = [(Vars,Types)]
type TSub = [(TVars,Types)]

-- 3.1
tsubst :: (TVars,Types) -> Types -> Types
tsubst (a, t) Ints = Ints
tsubst (a, t) (TVar b)
    | a == b    = t
    | otherwise = TVar b
tsubst (a, t) (Fun t1 t2) = Fun (tsubst (a, t) t1) (tsubst (a, t) t2)

csubst :: (TVars,Types) -> Constr -> Constr
csubst (a, t) (l, r)= (tsubst (a, t) l, tsubst (a, t) r)

getTVars :: Types -> [TVars]
getTVars Ints = []
getTVars (TVar a) = [a]
getTVars (Fun t1 t2) = getTVars t1 ++ getTVars t2

getTVarsCxt :: Cxt -> [TVars]
getTVarsCxt [] = []
getTVarsCxt ((x, t) : gamma) = getTVars t ++ getTVarsCxt gamma

-- 3.2
-- Helper to find a name not already used in the context/target type
getVar::[TVars]->TVars
getVar used = head [ [v] | v <- ['a' .. 'z'], [v] `notElem` used]

genConstrs::Cxt->Terms->Types->[Constr]
genConstrs gamma (Var x) ty = case lookup x gamma of
                    Nothing -> error("Var not in context: " ++ x)
                    Just t -> [(t, ty)]
genConstrs gamma (Num n) ty = [(Ints, ty)]
genConstrs gamma (Sub s t) ty =
    (ty, Ints) : genConstrs gamma s Ints ++ genConstrs gamma t Ints
genConstrs gamma (IfPos r s t) ty =
    genConstrs gamma r Ints ++ genConstrs gamma s ty ++ genConstrs gamma t ty
genConstrs gamma Y ty = 
        let a = getVar (getTVars ty ++ getTVarsCxt gamma)
            in [(ty, Fun (Fun (TVar a) (TVar a)) (TVar a))]
genConstrs gamma (App s t) ty =
        let a = getVar (getTVarsCxt gamma ++ getTVars ty)
            cs1 = genConstrs gamma s (Fun (TVar a) ty)
            cs2 = genConstrs gamma t (TVar a)
                in cs1 ++ cs2
genConstrs gamma (Abs x r) (Fun t1 t2) =
    genConstrs ((x, t1) : gamma) r t2
genConstrs gamma (Abs x r) ty =
    let used1 = getTVarsCxt gamma ++ getTVars ty
        a1 = getVar used1
        a2 = getVar (a1 : used1)
        cs = genConstrs ((x, TVar a1) : gamma) r (TVar a2)
            in (ty, Fun (TVar a1) (TVar a2)) : cs

-- 3.3
unify::[Constr]->[(TVars, Types)]
unify [] = []
unify ((Ints, Ints) : cs) = unify cs
unify ((TVar a, TVar b) : cs) | a == b = unify cs
unify ((TVar a, t): cs)
    | a `elem` getTVars t = error "Infinite Type error"
    | otherwise          = (a, t) : unify (map (csubst (a, t)) cs)
unify ((t, TVar a) : cs) = unify ((TVar a, t) : cs)
unify ((Fun l1 r1, Fun l2 r2) : cs) =
    unify ((l1, l2) : (r1, r2) : cs)
unify (c:_) = error ("Cannot unify: " ++ show c)

infer::Terms->Types
infer t = 
    let cs = genConstrs [] t (TVar "a")
        sub = unify cs
    in foldl (flip tsubst) (TVar "a") sub


-- --- Section 4 Reduction ---

-- 4.1
subst :: (Vars, Terms) -> Terms -> Terms
subst (x, t) (Var y)
    | x == y    = t
    | otherwise = Var y
subst (x, t) (Abs y body)
    | x == y    = Abs y body
    | otherwise = Abs y (subst (x, t) body)
subst (x, t) (App s1 s2) = App (subst (x, t)s1) (subst(x, t)s2)
subst (x, t) (Sub s1 s2) = Sub (subst (x, t)s1) (subst(x, t)s2)
subst (x, t) (IfPos s1 s2 s3) = IfPos (subst (x, t)s1) (subst(x, t)s2) (subst (x, t)s3)
subst _ (Num n) = Num n
subst _ Y = Y


-- 4.2
red :: Terms -> Terms
-- 4.2.1 Reduction Rules
red (App (Abs x s) t) = subst (x, t) s
red (Sub (Num m) (Num n)) = Num (m-n)
red (IfPos(Num n) s t) 
    | n > 0 = s
    | otherwise = t
red (App Y t) = App t (App Y t)


-- 4.2.2
red (App s t) = App (red s) (red t)
red (Sub s t) = Sub (red s) (red t)
red (IfPos r s t) = IfPos (red r) (red s) (red t)
red (Abs x s) = Abs x (red s)

-- 4.2.3                        
red (Var x) = Var x
red (Num n) = Num n
red Y = Y

-- 4.2.4
reds :: Terms -> Terms
reds t = let t' = red t
            in if t == t'
                then t
                else reds t'

-- --- Section 5 Frontend ---
main::IO ()
main = do
    -- Query user for filename
    putStr "Enter the file name: "
    fileName <- getLine

    -- Load file
    input <- readFile fileName

    -- Lex and Parse
    case parser (lexer input) of
        Right err -> putStrLn ("Parse/Lexical Error: " ++ err)
        Left term -> do
            putStrLn "Successfully parsed term."

            --Attempt type inference
            let computedType = infer term
            putStrLn ("Computed Type: " ++ show computedType)

            -- Attempt Reduction
            let finalResult = reds term
            putStrLn ("Result: " ++ show finalResult)