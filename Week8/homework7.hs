
import Data.List
import Data.Char

-- ---- Section 2 Extending the syntax of boolean formulas ----
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

-- ---- Section 3 Classification of Formulas ----
-- Helpers

evalList::Prop->[Env]->Bool
evalList prop [] = False
evalList prop (env:xs)
    | eval env prop     = True
    |otherwise  = evalList prop xs

extendEnv::[Env]->Vars->[Env]
extendEnv [] v = []
extendEnv (env:xs) v = ((v, False):env) : ((v, True):env) : extendEnv xs v

genEnvs::[Vars]->[Env]
genEnvs [] = [[]]
genEnvs (v:vs) = extendEnv (genEnvs vs) v

sat::Prop->Bool
sat p = evalList p (genEnvs (fv p))

-- ---- 3.1 Checking contradictions and tautologies----
contra::Prop->Bool
-- Not satisfiable
contra p = not (sat p)

tauto::Prop->Bool
-- Allways true
tauto p = not (sat (Not p))

-- ---- 3.2 Finding satisfying/refuting assignments----
findSat::Prop->Maybe Env
findSat p = case filter (\env-> eval env p) (genEnvs (fv p)) of
    [] -> Nothing
    (env:_)-> Just env

findRefute::Prop->Maybe Env
findRefute p = case filter (\env-> not (eval env p)) (genEnvs (fv p)) of
    [] -> Nothing
    (env:_)-> Just env

-- ---- 3.3 Classifying formulas----
classify::Prop->String
classify p
    |tauto p    = "tautology"
    |contra p   = "contradiction"
    |otherwise  = "contingency"

-- ----Section 4 Checking logical equivalence----
checkEq::Prop->Prop->Bool
checkEq p1 p2 = tauto (Iff p1 p2)

refuteEq::Prop->Prop->Maybe Env
refuteEq p1 p2 = findRefute (Iff p1 p2)

-- ----Section 5 Lexical Analysis----

-- binary operators
data BOps = AndOp | OrOp | ImpOp | IffOp | XorOp
    deriving (Show,Eq) -- the type of tokens
data Token = VSym Vars | CSym Bool | BOp BOps | NotOp | LPar | RPar
        | Err String -- auxiliary token to store unrecognized symbols
        | PB Prop -- auxiliary token to store parsed boolean expressions
    deriving (Show,Eq)

lexer::String->[Token]
lexer "" = []
lexer (x:xs) | isSpace x = lexer xs
-- Constants
lexer ('t':'t':xs) = CSym True : lexer xs
lexer ('f':'f':xs) = CSym False : lexer xs

-- Operators
lexer ('/':'\\':xs)= BOp AndOp : lexer xs
lexer ('\\':'/':xs)= BOp OrOp : lexer xs
lexer ('-':'>':xs)= BOp ImpOp : lexer xs
lexer ('<':'-':'>':xs)= BOp IffOp : lexer xs
lexer ('<':'+':'>':xs)= BOp XorOp : lexer xs
lexer ('!':xs) = NotOp : lexer xs

-- Parentheses
lexer ('(':xs) = LPar : lexer xs
lexer (')':xs) = RPar : lexer xs

-- Variables
lexer (x:xs) 
    | isUpper x = 
        let (v, ys) = span isAlphaNum xs
        in VSym (x:v) : lexer ys

-- Error
lexer (x:xs) = Err [x] : lexer xs

-- ---- Section 6 Parser ----
rank::BOps->Integer
rank AndOp =3
rank OrOp = 2
rank ImpOp =2
rank XorOp = 2
rank IffOp =0

-- ---- 6.1 Shift-reduce helper function ----
sr::[Token]->[Token]->[Token]
sr (Err e:s) q = [Err e]
sr stack@(PB e2 : BOp op : PB e1 : s)(BOp op2 : q) 
    | rank op < rank op2 = sr (BOp op2 : stack) q
-- Variable
sr (VSym x : s) q = sr (PB(Var x):s)q

-- Const
sr (CSym x : s) q = sr (PB(Const x):s)q

-- Operator
sr (PB e : NotOp : s) q = sr (PB (Not e) : s) q
sr (PB e2 : BOp AndOp : PB e1 : s) q = sr (PB (And e1 e2) : s)q
sr (PB e2 : BOp OrOp : PB e1 : s) q = sr (PB (Or e1 e2) : s)q
sr (PB e2 : BOp ImpOp : PB e1 : s) q = sr (PB (Imp e1 e2) : s)q
sr (PB e2 : BOp IffOp : PB e1 : s) q = sr (PB (Iff e1 e2) : s)q
sr (PB e2 : BOp XorOp : PB e1 : s) q = sr (PB (Xor e1 e2) : s)q
sr (RPar : PB e : LPar : s) q = sr (PB e : s) q

-- Shift phase
sr s (q:qs) = sr (q:s) qs
sr s [] = s

-- ---- 6.2 Parsing with error handling ----
parseProp::String->Either Prop String
parseProp str = case sr [] (lexer str) of 
    [PB e] -> Left e
    [Err e] -> Right ("Lexical error: " ++ e)
    str -> Right ("Parse error: " ++ show str)

-- ---- Section 7 An front-end with terminal I/O ----
main::IO ()
main = do 
    line <- getLine

    if line == "quit" then return()
        else do
            case parseProp line of
                Right err -> putStrLn err
                Left prop -> do
                    let c = classify prop
                    putStrLn c
                    
                    if c == "contingency"
                        then do
                            print (findSat prop)
                            print (findRefute prop)
                        else return ()
            main