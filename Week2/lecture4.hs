-- Extra credit problem on quiz
filterDigits::[Integer]->[Integer]
filterDigits xs = [x |x<-xs, 0 <=  x, x<=9]

filterDigits'::[Integer]->[Integer]
filterDigits' [] = []
filterDigits'(x:xs) = if 0 <= x && x <= 9
        then x : filterDigits' xs
        else filterDigits' xs

-- or 
filterDigits''::[Integer]->[Integer]
filterDigits'' (x:xs)
    |0<=x&&x<=9= x:filterDigits'' xs
    |otherwise =filterDigits'' xs

-- Note: head will be stored in x and tail in xs

addList::[Integer]->Integer
addList []= 0
addList (x:xs) = x+addList xs

nullList::[Integer]->Integer
nullList [] = 1
nullList(x:xs) = x+nullList xs

andList::[Bool]->Bool
andList [] = True   -- True as the base case becuase the neutral elememt is true. If with OR, you take False as the base case
andList (x:xs) = x && andList xs

-- Decide whether every integer in the list is odd
allOdds::[Integer]->Bool
allOdds [] = True
allOdds (x:xs) = odd x && allOdds xs

-- Go through a list if you find a number divisible by 7 then return. Else -1
find7::[Integer]->Integer
find7 [] = -1
find7 (x:xs) = if mod x 7 == 0
        then x
        else find7 xs

-- Same but return the index of the list
find7index::[Integer]->Integer
find7index xs = search 0 xs where
    search i [] = -1
    search i (y:ys) = if mod y 7 == 0
        then i
        else search (i+1) ys

-- Go through a list, if you find a number x divisible by 7, return
    -- else return nothing
find7Maybe::[Integer]->Maybe Integer
find7Maybe [] = Nothing
find7Maybe (x:xs) = if mod x 7 == 0
        then Just x
        else find7Maybe xs

-- add x to maybe integer y if y is nothing, add zero
addMaybe::Integer->Maybe Integer->Integer
addMaybe x Nothing = x
addMaybe x (Just y) = x+y

addMaybes::[Maybe Integer]->Integer
addMaybes [] = 0
addMaybes (Nothing:xs) = addMaybes xs
addMaybes (Just y:xs) = y+addMaybes xs