import Data.Char
import Data.List

type Vars = String
type Value = Integer
type VEnv = [(Vars, Value)]

data FunDef = FunDef {name::String,
                        vars::[Vars],
                        body::AExpr}
    deriving Show

type FEnv = [FunDef]
type Env = ()

funDefEx::FunDef
funDefEx = FunDef "F" ["X"] (Add(Exp(Var "x")(Num 2))(Num 1))

parseFunDef::[Token]->FunDef
parseFunDef (NSym fname : xs) =
    let p (VSym _) = True
        p _ = False
        unVSym (VSym x) = x
        (vs, EqSym : rest) = span p xs 
    in case parseAExpr rest of
        Left expr -> FunDef fname (map unVSym vs) expr
        Right err -> error err

data AExpr = Var Vars | Num Integer | Add AExpr AExpr | Sub AExpr AExpr
                                    | Mul AExpr AExpr | Div AExpr AExpr | Exp AExpr AExpr
                                    | FunCall String [AExpr]
    deriving Show
data BOps = AddOp | SubOp | MulOp | DivOp | ExpOp
    deriving Show
data Commands = QuitC | PrintC | LoadC
    deriving Show
data Token = VSym String | CSym Integer | NSym String | LPar | RPar | BOp BOps | EqSym | Err String
                | PA AExpr | Command Commands | AssignOp | Comma
                | ArgList [AExpr]
    deriving Show

{-main =
    getLine >>= (\x -> case parseAExpr (lexer x) of
                    Left expr -> putStrLn (show (eval [] expr))
                    Right err -> putStrLn err) -}

-- Read (input) Eval (input) Print (the evaluation) Loop (do it again)
repl::Env->IO()
    {-
    repl env = do
    x <- getLine
    if x == "quit"
        then return ()
        else do
            let y = lexer x
            case parseAExpr y of
                Left expr -> putStrLn (show (eval [] expr))
                Right err -> putStrLn err
            repl env
    will be turned into:  -}

    {- Comment:
    Right now we can evaluate the expression and quit but we want to add:
    - Store expression results into variables (v := ....)
    - Display the current environment (:print)
    - Read preset variable-value tale from a file (:load)
            - Will query the  for the filename
    
    -}

execAssign::[Token]->Env->Env
execAssign (VSym v : AssignOp : ts) env =
        case parseAExpr ts of 
            Left expr -> (v, eval env expr) : env
            Right err -> env
execAssign _ env = env

repl env = getLine >>= (\inp-> parse (lexer inp)) where
    parse [Command QuitC] = return ()
    parse [Command PrintC] = do putStrLn (show env) >> repl env -- It is like chaining two do actions
    parse (VSym v : AssignOp : ts) =
        case parseAExpr ts of 
            Left expr -> repl ((v, eval env expr) : env)
            Right err -> putStrLn err >> repl env 
    parse [Command LoadC] = do
        putStr "Enter name of file"
        filename <- getLine
        filecontents <- readFile filename
        let filelines = lines filecontents
        let filelexed = map lexer filelines
        let newenv = foldl (flip execAssign) [] filelexed
        repl newenv
    parse ts =
        case parseAExpr ts of 
            Left expr -> putStrLn (show (eval env expr))
            Right err -> putStrLn err
    
main = repl []

-- For every grammar rule the parser checks if it can be reduced
parseAExpr::[Token]->Either AExpr String
parseAExpr ts = case sr [] ts of
    [PA e] -> Left e
    [Err e] -> Right ("LLexical error:" ++ e)
    st -> Right("Parse error:" ++ show st)

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
-- Function Call
sr (LPar : NSym f : s) q = sr (ArgList [] : NSym f : s)q
sr (Comma : PA e : ArgList es : s)q = sr (ArgList (e:es) : s)q
sr (RPar : PA e : ArgList es: NSym f : s)q = 
                        sr (PA(FunCall f (reverse (e:es))) : s)q
sr (RPar :ArgList es : NSym f : s)q = sr (PA (FunCall f (reverse es)): s)q
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

-- Command
lexer (':':xs) | isPrefixOf "quit" xs = [Command QuitC]
lexer (':':xs) | isPrefixOf "print" xs = [Command PrintC]
lexer (':':xs) | isPrefixOf "load" xs = [Command LoadC]
lexer (':':'=':xs) = AssignOp :lexer xs

-- Constants
lexer (x:xs) | isDigit x = CSym (read n) : lexer ys
    where (n, ys) = span isDigit (x:xs)
-- Variables
lexer (x:xs) | isLower x = VSym v : lexer ys
    where (v, ys) = span isAlphaNum (x:xs)
-- Function name
lexer (x:xs) | isLower x = NSym n : lexer ys
    where (n, ys) = span isAlphaNum (x:xs)
-- Operators and parens
lexer ('+':xs)= BOp AddOp : lexer xs
lexer ('-':xs)= BOp SubOp : lexer xs
lexer ('*':xs)= BOp MulOp : lexer xs
lexer ('/':xs)= BOp DivOp : lexer xs
lexer ('^':xs)= BOp ExpOp : lexer xs

lexer (',':xs)= Comma : lexer xs
lexer ('(':xs)= LPar : lexer xs
lexer (')':xs)= RPar : lexer xs
lexer ('=':xs)= EqSym : lexer xs
lexer xs = [Err (drop 10 xs)]

lookupFun::String->FEnv->FunDef
lookupFun f [] = error "No function named" ++ f ++ "found!"
lookupFun f (fd : fenv) 
    | name fd == f = fd
    | otherwise = lookupFun f fenv

eval:: Env->AExpr->Value
eval env (venv, _) (Var v) = case lookup v env of
        Just x -> x 
        Nothing -> error "Variable not found!"
eval env (Num c) = c
eval env (Mul e1 e2) = eval env e1 * eval env e2
eval env (Add e1 e2) = eval env e1 + eval env e2
eval env (Sub e1 e2) = eval env e1 - eval env e2
eval env (Div e1 e2) = eval env e1 `div` eval env e2
eval env@(_, fenv) (FunCall f es) =
    let funDef = lookupFun f fenv
        args = map (eval env) es
        callenv = zip (vars fundef) args
    in eval (callEnv, fenv) (body fundef)