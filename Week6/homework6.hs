-- Homework 6 Syntax and Logic

-- ------------WarmUp Problems------------
-- Question 1
mapAppend::(a->[b])->[a]->[b]
mapAppend f xs = concat (map f xs)

-- Question 2
addLetter::Char->[String]->[String]
addLetter c [] = []
addLetter c (x:xs) = (c:x) : addLetter c xs

-- Question 3
addLetters::[Char]->[String]->[String]
addLetters c str = mapAppend (\x-> addLetter x str) c

-- Question 4
makeWords::[Char]->Integer->[String]
makeWords str 0 = [""]
makeWords str l = addLetters str (makeWords str (l-1))

-- Question 5
update::(Eq a)=>(a,b)->[(a,b)]->[(a,b)]
update (key, value) [] = [(key, value)]
update (key, value) ((k,v):xs)
    |key == k   = (key, value) : xs
    |otherwise  = (k,v) : update (key, value) xs

-- ------------Propositions------------

type Vars = String
type Env = [(Vars,Bool)]
data Prop = Var Vars | Const Bool | And Prop Prop | Or Prop Prop | Not Prop
    deriving (Show,Eq)

-- Question 1

fv::Prop->[Vars]
fv (Var v) = [v]
fv (Const _) = []
fv (And p1 p2) = removeDup (fv p1 ++ fv p2)
fv (Or p1 p2) = removeDup (fv p1 ++ fv p2)
fv (Not p1) = fv p1

removeDup:: Eq a => [a] -> [a]
removeDup [] = []
removeDup (x:xs)
    |x `elem` xs = removeDup xs
    |otherwise = x : removeDup xs

-- Question 2
countOccurs::Vars->Prop->Integer
countOccurs e (Var v)
    |e == v     = 1
    |otherwise  = 0
countOccurs _ (Const _) = 0
countOccurs e (And p1 p2) = countOccurs e p1 + countOccurs e p2
countOccurs e (Or p1 p2) = countOccurs e p1 + countOccurs e p2
countOccurs e (Not p1) = countOccurs e p1

-- Question 3
setTrue::Vars->Prop->Prop
setTrue e (Var v)
    |e == v = Const True
    |otherwise  = Var v
setTrue _ (Const c) = Const c
setTrue e (And p1 p2) = And (setTrue e p1) (setTrue e p2)
setTrue e (Or p1 p2) = Or (setTrue e p1) (setTrue e p2)
setTrue e (Not p1) = Not (setTrue e p1)

-- Question 4
lookUp::Vars->Env->Bool
lookUp e [] = error "There is an error"
lookUp e ((key, val):xs)
    |e == key  = val
    |otherwise  = lookUp e xs

-- Question 5
eval::Env->Prop->Bool
eval env (Var v) = lookUp v env
eval env (Const c) = c
eval env (Not p) = not (eval env p)
eval env (And p1 p2) = eval env p1 && eval env p2
eval env (Or p1 p2) = eval env p1 || eval env p2

-- Question 6
evalList::Prop->[Env]->Bool
evalList prop [] = False
evalList prop (env:xs)
    | eval env prop     = True
    |otherwise  = evalList prop xs

-- Question 7
extendEnv::[Env]->Vars->[Env]
extendEnv [] v = []
extendEnv (env:xs) v = ((v, False):env) : ((v, True):env) : extendEnv xs v

-- Question 8
genEnvs::[Vars]->[Env]
genEnvs [] = [[]]
genEnvs (v:vs) = extendEnv (genEnvs vs) v

-- Question 9
sat::Prop->Bool
sat p = evalList p (genEnvs (fv p))

-- Question 10
checkEq::Prop->Prop->Bool
checkEq f1 f2 = all (\env -> eval env f1 == eval env f2) (genEnvs (removeDup (fv f1 ++ fv f2)))
