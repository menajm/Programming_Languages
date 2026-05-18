import Data.Char
import Data.List

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

sr (VSym x : s) (Dot : q) = sr (Dot : VSym x : s) q
sr (VSym x : s) q = sr (PT (Var x) : s) q
sr (CSym c : s) q = sr (PT (Num c) : s) q
sr (YComb : s) q  = sr (PT Y : s) q

sr _ (Err e : _) = Right ("Lexical Error: " ++ e)

sr (RPar : PT m : LPar : s) q = sr (PT m : s) q

sr (PT n : PT m : s) q
    | null q || isNotOp (head q) = sr (PT (App m n) : s) q
    where 
        isNotOp tok = case tok of
            VSym _ -> True
            CSym _ -> True
            LPar   -> True
            _      -> False

sr (PT m : Dot : VSym x : Backslash : s) q
    | null q || notFollowsBody (head q) = sr (PT (Abs x m) : s) q
    where
        notFollowsBody tok = case tok of
            VSym _ -> False
            CSym _ -> False
            LPar   -> False
            SubOp  -> False
            _      -> True

sr (PT t : SubOp : PT s : s') q = sr (PT (Sub s t) : s') q
sr (PT u : ElseK : PT t : ThenK : PT s : IfPositiveK : s') q
    | null q || notFollowsElse (head q) = sr (PT (IfPos s t u): s') q
    where
        notFollowsElse tok = case tok of
            SubOp  -> False
            VSym _ -> False
            CSym _ -> False
            LPar   -> False
            _      -> True

sr stack [] = Right ("Parse Error: " ++  show stack)
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
getTVars (Fun t1 t2) = nub (getTVars t1 ++ getTVars t2)

getTVarsCxt :: Cxt -> [TVars]
getTVarsCxt gamma = nub [v | (_, t) <- gamma, v <- getTVars t]



-- 3.2

genConstrs :: Cxt -> Terms -> Types -> [Constr]
-- Variable
genConstrs gamma (Var x) ty =
    case lookup x gamma of
        Nothing -> error ("Var not in context: " ++ x)
        Just t  -> [(t, ty)]
-- Number
genConstrs gamma (Num _) ty = [(ty, Ints)]
-- Sub
genConstrs gamma (Sub s t) ty =
    (ty, Ints) : genConstrs gamma s Ints ++ genConstrs gamma t Ints
-- IfPositive
genConstrs gamma (IfPos r s t) ty =
    genConstrs gamma r Ints ++ genConstrs gamma s ty ++ genConstrs gamma t ty
-- Y combinator: Standard fixed-point type
genConstrs gamma Y ty =
    let fresh = "v" ++ show (length gamma)
    in [(ty, Fun (Fun (TVar fresh) (TVar fresh)) (TVar fresh))]

-- Application: intermediate type for the function domain
genConstrs gamma (App s t) ty =
    let argTy = "t_app" ++ show (length gamma)
    in genConstrs gamma s (Fun (TVar argTy) ty) ++ genConstrs gamma t (TVar argTy)

-- Abstraction: Bind x to a type and recurse
genConstrs gamma (Abs x r) ty =
    let inTy  = "ty_" ++ x
        outTy = "res_" ++ x
    in (ty, Fun (TVar inTy) (TVar outTy)) : genConstrs ((x, TVar inTy) : gamma) r (TVar outTy)

-- 3.3
unify::[Constr]->[(TVars, Types)]
unify [] = []
unify ((s,t):cs)
  | s == t = unify cs
-- variable case
unify ((TVar x, t):cs)
  | occurs x t = error "Infinite Type error"
  | otherwise =
      let sub = (x, t)
          cs' = map (csubst sub) cs
          rest = unify cs'
      in sub : map (\(y, ty) -> (y, tsubst sub ty)) rest
-- symmetric case
unify ((t, TVar x):cs) =
  unify ((TVar x, t):cs)
-- function case
unify ((Fun s1 s2, Fun t1 t2):cs) =
  unify ((s1,t1):(s2,t2):cs)
-- mismatch
unify _ = error "Type error"

occurs :: TVars -> Types -> Bool
occurs x t = x `elem` getTVars t


infer::Terms->Types
infer t =
    let target = TVar "final_result"
        cs = genConstrs [] t target
        sub = unify cs
    in foldl (\acc s -> tsubst s acc) target sub



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
red (App (Abs x s) t)     = subst (x, t) s
red (App Y t)             = App t (App Y t)
red (Sub (Num m) (Num n)) = Num (m - n)
red (IfPos (Num n) s t)   = if n > 0 then s else t

red (App s t) = App (red s) (red t)
red (Sub s t) = Sub (red s) (red t)
red (IfPos r s t) = IfPos (red r) (red s) (red t)
red (Abs x s) = Abs x (red s)

-- Base cases
red t = t

-- 4.2.4
reds :: Terms -> Terms
reds t = let t' = red t
            in if t == t'
                then t
                else reds t'



-- --- Section 5 Frontend ---
homeworkMain::IO ()
homeworkMain = do
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
