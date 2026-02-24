-- Folds
findReverse::[(String, String)]-> Maybe String
findReverse [] = Nothing
findReverse ((x,y):xs) 
    |x == reverse y  = Just x
    | otherwise     = findReverse xs

concatStrings::[Either Double String]->String
concatStrings [] = ""
concatStrings (Left n:xs) = concatStrings xs
concatStrings (Right x:xs) = x ++ concatStrings xs

concatList::[String]-> String
concatList [] = ""
concatList (x:xs) = x ++ concatList xs

doubleList::[Integer]->[Integer]
doubleList [] = []
doubleList (x:xs) = 2*x : doubleList xs


sumList::[Integer]->Integer
sumList [] = 0
sumList (x:xs) = x + sumList xs

-- All have a recusive call to the function which is the same type as where the arrow last points to
-- What is the base? And what is the rule which combines the head of the lsit and the recursive call

-- For example, in sumList, lambda headOfList recursiveCall -> headOfList + recurseiveCall
    -- Follow the last line of the function to do that

fold::(a->b->b)->b->[a]->b
-- ^Lambda function
-- rf recursive, bc is base case
fold rf bc [] = bc
fold rf bc (x:xs) = rf x (fold rf bc xs)


-- fold (recursive) (base) is how it works

-- For sumList- fold (\x y->x+y) 0 [1..5]
    -- Instead of the lambda part it is also just +

--concactList- fold (++) "" ["Hello", "list", "strings"]

-- doubleList- fold (\x y -> 2*x : y) [] [1..5]

-- findReverse- fold (\x -> y-> if fst x == reverse (snd x) 
        -- then Just (fst x) else y) (Nothing) [("hey", "you"), ("dog", "god")]

-- concatStrings- fold (\x y -> either (\n->y) (\s->s++y) x) "" [Left 5.1, Right "hi", Right "hey", Left 0]

findEven::[Integer]->Maybe Integer
findEven [] = Nothing
findEven (x:xs) = if even x then Just x else findEven xs
-- Refer to notes on that one ^