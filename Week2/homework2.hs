
-- Homework 2 Problems

-- ------------------Bubble Sort-------------------------
-- Question 1
bubble::Ord a=>[a]->[a]
bubble [] = []
bubble [x] = [x]
bubble (x:y:xs)
    | x <= y = x:bubble(y:xs)
    |otherwise = y:bubble(x:xs)

-- Question 2
bubbleSort::Ord a=>[a]->[a]
bubbleSort xs =
    let x = bubble xs
    in if x == xs
        then xs
        else bubbleSort x

-- -------------Generating, searching and replacing strings----

-- Question 1
-- Helper function isPrefix
isPrefix::(Eq a) => [a]->[a]->Bool
isPrefix [] ys = True
isPrefix (x:xs) [] = False
isPrefix (x:xs) (y:ys) =
    if x == y
        then isPrefix xs ys
        else False
-- -------------------------------
isSubstring::String->String->Bool
isSubstring [] _ = True
isSubstring _ [] = False
isSubstring ss (x:xs) = 
    if isPrefix ss (x:xs)
        then True
        else isSubstring ss xs

-- Question 2
-- Helper function
genTails::String->[String]
genTails [] = []
genTails (x:xs) = (x:xs):genTails xs
-- -----------------
genPrefix::String->[String]
genPrefix "" = []
genPrefix s = map reverse(genTails(reverse s))

-- Question 3
genSubstrings::String->[String]
genSubstrings s = "" : concat (map genPrefix(genTails s))

-- Question 4
replacePrefix::(String, String)->String->String
replacePrefix (old, new) str = 
    let result = new ++ drop (length old) str
    in result

-- Question 5
replaceString::(String, String)->String->String
replaceString (ls, rs) [] = [] 
replaceString (ls, rs) str
    | ls `isPrefix` str = rs ++ drop (length ls) str
    | otherwise = head str:replaceString(ls, rs) (tail str)

-- -----------------A simple cypher ------------

-- Question 1
lookUp::Char->[(Char, Char)]->Char
lookUp x ((key, value):remainingPairs)
    |x == key = value
    |otherwise = lookUp x remainingPairs

-- Question 2
encode::[(Char, Char)]->String->String
encode table [] = []
encode table (x:xs) =
        lookUp x table:encode table xs 

-- Question 3
makeTable::String->String->[(Char,Char)]
makeTable [] _ = []
makeTable _ [] = []
makeTable (x:xs) (y:ys) =
    (x,y):makeTable xs ys
