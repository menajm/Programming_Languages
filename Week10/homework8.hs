import Data.Char
import Data.List

-- --- 2. IMP: Language Definition ---
-- 2.1 Types
    --
-- 2.2 Syntax
type Vars = String -- Variables
type Value = Integer -- Values
type Env = [(Vars,Value)]

data AExpr = Var Vars | Num Value -- Arithmetic expressions
    | Add AExpr AExpr | Sub AExpr AExpr | Mul AExpr AExpr
    | Div AExpr AExpr | Mod AExpr AExpr | Exp AExpr AExpr
    deriving (Eq,Show)
data BExpr = Const Bool -- Boolean expressions
    | And BExpr BExpr | Or BExpr BExpr | Not BExpr
    | Eq AExpr AExpr | Lt AExpr AExpr | Lte AExpr AExpr
    | Neq AExpr AExpr | Gt AExpr AExpr | Gte AExpr AExpr
    deriving (Eq,Show)
data Instr = Assign Vars AExpr -- assignment
    | IfThen BExpr Instr -- conditional
    | IfThenElse BExpr Instr Instr -- another conditional
    | While BExpr Instr -- looping construct
    | Do [Instr] -- a block of several instructions
    | Nop -- the "do nothing" instruction
    | Return AExpr -- the final value to return
    deriving (Eq,Show)


-- --- 3. Lexical analysis ---
data Keywords = IfK | ThenK | ElseK | WhileK | NopK | ReturnK
    deriving (Eq,Show)
data BOps = AddOp | SubOp | MulOp | DivOp | ModOp | ExpOp
    | AndOp | OrOp | EqOp | NeqOp
    | LtOp | LteOp | GtOp | GteOp
    deriving (Eq,Show)
data Token = VSym String | CSym Integer | BSym Bool
    | LPar | RPar | LBra | RBra | Semi
    | BOp BOps | NotOp | AssignOp
    | Keyword Keywords
    | Err String
    | PA AExpr | PB BExpr | PI Instr | Block [Instr]
    deriving (Eq,Show)

lexer::String->[Token]
lexer "" = []
lexer (x:xs) | isSpace x = lexer xs

-- Operators
lexer (':':'=':xs) = AssignOp : lexer xs
lexer ('+':xs) = BOp AddOp : lexer xs
lexer ('-':xs) = BOp SubOp : lexer xs
lexer ('*':xs) = BOp MulOp : lexer xs
lexer ('/':xs) = BOp DivOp : lexer xs
lexer ('%':xs) = BOp ModOp : lexer xs
lexer ('^':xs) = BOp ExpOp : lexer xs
lexer ('&':'&':xs) = BOp AndOp : lexer xs
lexer ('|':'|':xs) = BOp OrOp : lexer xs
lexer ('=':'=':xs) = BOp EqOp : lexer xs
lexer ('!':'=':xs) = BOp NeqOp : lexer xs
lexer ('<':'=':xs) = BOp LteOp : lexer xs
lexer ('<':xs) = BOp LtOp : lexer xs
lexer ('>':'=':xs) = BOp GteOp : lexer xs
lexer ('>':xs) = BOp GtOp : lexer xs
lexer ('!':xs) = NotOp : lexer xs

-- Punctuation Marks
lexer ('(':xs) = LPar : lexer xs
lexer (')':xs) = RPar : lexer xs
lexer ('{':xs) = LBra : lexer xs
lexer ('}':xs) = RBra : lexer xs
lexer (';':xs) = Semi : lexer xs

-- Keywords
lexer (x:xs) | isLower x = 
    let (word, rest) = span isAlphaNum (x:xs)
    in case word of
        "while" ->      Keyword WhileK : lexer rest
        "if" ->         Keyword IfK : lexer rest
        "then" ->       Keyword ThenK : lexer rest
        "else" ->       Keyword ElseK : lexer rest
        "nop" ->        Keyword NopK : lexer rest
        "return" ->     Keyword ReturnK : lexer rest
        -- Boolean Constants
        "tt" ->         BSym True : lexer rest
        "ff" ->         BSym False : lexer rest
        -- Variable
        _ ->            VSym word : lexer rest

-- Constants
lexer (x:xs) | isDigit x =
    let (num, ys) = span isDigit (x:xs) 
    in CSym (read num): lexer ys

lexer xs = [Err (take 10 xs)]

-- --- 4. Parsing ---
-- To use PEMDAS...
rank::BOps->Integer
rank AddOp = 10
rank SubOp = 10
rank MulOp = 20
rank DivOp = 20
rank ModOp = 20
rank ExpOp = 30

rank EqOp = 7
rank NeqOp = 7
rank LtOp = 8
rank LteOp = 8
rank GtOp = 8
rank GteOp = 8
rank AndOp = 5
rank OrOp = 4


sr::[Token]->[Token]->[Token]

sr (VSym x : s) q = sr (PA(Var x):s)q
sr (CSym x : s) q = sr (PA(Num x):s)q
sr (BSym b : s) q = sr (PB(Const b):s)q

sr (RPar : PA e : LPar : s)q = sr (PA e : s)q
sr (RPar : PB b : LPar : s)q = sr (PB b : s)q

sr stack@(PA _ : BOp op1 : PA _ : s) (BOp op2 : q)
    | rank op1 < rank op2 =
        sr (BOp op2 : stack) q
sr stack@(PB _ : BOp op1 : PB _ : s) (BOp op2 : q)
    | rank op1 < rank op2 =
        sr (BOp op2 : stack) q

sr (PA e2 : BOp AddOp : PA e1 : s) q = sr (PA (Add e1 e2) : s)q
sr (PA e2 : BOp SubOp : PA e1 : s) q = sr (PA (Sub e1 e2) : s)q
sr (PA e2 : BOp MulOp : PA e1 : s) q = sr (PA (Mul e1 e2) : s)q
sr (PA e2 : BOp DivOp : PA e1 : s) q = sr (PA (Div e1 e2) : s)q
sr (PA e2 : BOp ModOp : PA e1 : s) q = sr (PA (Mod e1 e2) : s)q
sr (PA e2 : BOp ExpOp : PA e1 : s) q = sr (PA (Exp e1 e2) : s)q

sr (PB b : NotOp : s)q = sr (PB (Not b) : s)q
sr (PB b2 : BOp AndOp : PB b1 : s)q = sr (PB (And b1 b2) : s)q
sr (PB b2 : BOp OrOp : PB b1 : s)q = sr (PB (Or b1 b2) : s)q
sr (PA e2 : BOp EqOp : PA e1 : s)q = sr (PB (Eq e1 e2) : s)q
sr (PA e2 : BOp NeqOp : PA e1 : s)q = sr (PB (Neq e1 e2) : s)q
sr (PA e2 : BOp LtOp : PA e1 : s)q = sr (PB (Lt e1 e2) : s)q
sr (PA e2 : BOp LteOp : PA e1 : s)q = sr (PB (Lte e1 e2) : s)q
sr (PA e2 : BOp GtOp : PA e1 : s)q = sr (PB (Gt e1 e2) : s)q
sr (PA e2 : BOp GteOp : PA e1 : s)q = sr (PB (Gte e1 e2) : s)q


sr (Semi : PA e : AssignOp : PA (Var v) : s)q = sr (PI (Assign v e) : s)q

sr (PI i2 : Keyword ElseK : PI i1 : Keyword ThenK : PB c : Keyword IfK : s) q = 
        sr (PI (IfThenElse c i1 i2) : s) q
sr (PI i : Keyword ThenK : PB c : Keyword IfK : s) (Keyword ElseK : q) = 
        sr (Keyword ElseK : PI i : Keyword ThenK : PB c : Keyword IfK : s)q
sr (PI i : Keyword ThenK : PB c : Keyword IfK : s) [] =
    sr (PI (IfThen c i) : s) []
sr (PI i : Keyword ThenK : PB c : Keyword IfK : s) (Semi : q) =
    sr (Semi : PI (IfThen c i) : s) q
sr (PI i : Keyword ThenK : PB c : Keyword IfK : s) (RBra : q) =
    sr (RBra : PI (IfThen c i) : s) q

sr (Semi : PA e : Keyword ReturnK : s)q = sr (PI (Return e) : s)q

sr (Semi : Keyword NopK : s)q = sr (PI Nop : s)q

-- Start a block when we see the first instruction after '{'
sr (PI i : LBra : s) q =
    sr (Block [i] : LBra : s) q
-- Extend the block with more instructions
sr (PI i : Block is : s) q =
    sr (Block (i:is) : s) q
-- Close a non-empty block
sr (RBra : Block is : LBra : s) q =
    sr (PI (Do (reverse is)) : s) q
-- Empty block
sr (RBra : LBra : s) q =
    sr (PI (Do []) : s) q

sr (PI i : PB c : Keyword WhileK : s)q = sr (PI (While c i) : s)q
sr (PI i : PB c : Keyword WhileK : s) (Semi : q) = sr (Semi : PI (While c i) : s) q
sr (PI i : PB c : Keyword WhileK : s) (RBra : q) = sr (RBra : PI (While c i) : s) q
sr (PI i : PB c : Keyword WhileK : s) [] = sr (PI (While c i) : s) []

sr (Err e : s) q = [Err e]
sr s (x : q) = sr (x : s) q 
sr s [] = s


readProg :: [Token] -> Either [Instr] String
readProg ts =
    case sr [] (LBra : ts ++ [RBra]) of
        [PI (Do is)] -> Left is
        _            -> Right "Parse error"


-- --- 5. Evaluating Expressions ---
update :: (Vars, Integer) -> Env -> Env
update (x,v) [] = [(x,v)]
update (x,v) ((y,val):ys)
    | x == y    = (x,v) : ys
    | otherwise = (y,val) : update (x,v) ys

evala :: Env -> AExpr -> Integer
evala env (Num n) = n
evala env (Var v) = case lookup v env of
    Just val -> val
    Nothing -> error("Variable not found:" ++ v)
evala env (Add e1 e2) = evala env e1 + evala env e2
evala env (Sub e1 e2) = evala env e1 - evala env e2
evala env (Mul e1 e2) = evala env e1 * evala env e2
evala env (Div e1 e2) = evala env e1 `div` evala env e2
evala env (Mod e1 e2) = evala env e1 `mod` evala env e2
evala env (Exp e1 e2) = evala env e1 ^ evala env e2

evalb :: Env -> BExpr -> Bool
evalb env (Const c) = c
evalb env (And b1 b2) = evalb env b1 && evalb env b2
evalb env (Or b1 b2) = evalb env b1 || evalb env b2
evalb env (Not b) = not (evalb env b)
evalb env (Eq e1 e2) = evala env e1 == evala env e2
evalb env (Neq e1 e2) = evala env e1 /= evala env e2
evalb env (Lt e1 e2) = evala env e1 < evala env e2
evalb env (Lte e1 e2) = evala env e1 <= evala env e2
evalb env (Gt e1 e2) = evala env e1 > evala env e2
evalb env (Gte e1 e2) = evala env e1 >= evala env e2

-- ---6. Executing Instructions---
exec::Instr->Env->Env
exec (Assign x e) env = update (x, evala env e) env
exec Nop env = env
exec (Return e) env = update ("", evala env e) env
exec (Do is) env = execList is env
exec (IfThen b i) env = if evalb env b
                            then exec i env else env
exec (IfThenElse b i1 i2) env = if evalb env b
                                    then exec i1 env else exec i2 env
exec (While b i) env = case lookup "" env of
                                Just _ -> env
                                Nothing -> if evalb env b
                                    then let env' = exec i env
                                    in case lookup "" env' of
                                        Just _  -> env'
                                        Nothing -> exec (While b i) env'
                                    else env

execList::[Instr]->Env->Env
execList [] env = case lookup "" env of
        Just v  -> [("", v)]
        Nothing -> env
execList (i:is) env = 
    case lookup "" env of
        Just v -> [("", v)]
        Nothing -> 
            let env' = exec i env
            in execList is env'

run :: [Instr] -> Integer
run p = case lookup "" (execList p []) of
    Just x -> x
    Nothing -> error "No value returned."

-- ---7. Loading the Source File---
main::IO ()
main = do
    putStr "Enter file name: "
    filename <- getLine
    contents <- readFile filename

    let tokens = lexer contents
    case readProg tokens of
        Left prog -> do
            let result = run prog
            putStrLn ("Result: " ++ show result)
        Right err -> putStrLn ("Error: " ++ err)
