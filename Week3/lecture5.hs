import Data.Time.Format.ISO8601 (yearFormat)
-- Maybe type can be used in pattern matching when it is an input
-- Usually for when you aren't sure about the output

--x^y, but if y is negative, it should output an error on String
power::Integer->Integer->Either Integer String
power x y
    | y>= 0 = Left (x^y)
    | y<0 = Right "The exponent can't be a negative"

-- Takes a string or an integer and ouputs either double the length of the string
-- or the square of the integer
doubleOrSquare::Either String Integer->Integer
doubleOrSquare (Left x) = 2 * fromIntegral(length x)
doubleOrSquare (Right y) = y^2

-- Using Either type
concatAll::[Either String Integer]->String
concatAll [] = ""
concatAll(Left s:xs) = s ++ concatAll xs
concatAll(Right n:xs) = show n ++ concatAll xs

-- Insertion Sort algorithm inserts a new integer into the correct place inside an already
-- sorted list
-- insert:: Ord a => a ->[a]->[a] (This will work with other data types)
insert:: Integer->[Integer]->[Integer]
insert x [] = [x]
insert x (y:ys) = if x < y then x:y:ys else y:insert x ys
-- | x <= y = x:y:ys
-- | otherwise = y:insert x ys

-- insertionSort:: Ord a=>[a]->[a]
insertionSort::[Integer]->[Integer]
insertionSort [] = []
insertionSort (x:xs) = insert x (insertionSort xs)

-- Takes two lists that are alreadsy sorted, and merges them together
merge::Ord a=> [a]->[a]->[a]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys)
    | x <= y = x:merge xs (y:ys)
    | otherwise = y : merge (x:xs) ys

split::[a] ->([a], [a])
split [] = ([], [])
split [x] = ([x], [])
split (x:y:ys) = ( x:ys1, y:ys2) where (ys1, ys2) = split ys
    -- let (ys1, ys2) = split ys
    -- in (x:ys1:y:ys2)

mergeSort::Ord a=>[a]->[a]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort xs =
    let (ys, zs) = split xs
    in merge(mergeSort ys)(mergeSort zs)
    -- let p = split xs
    --    in merge(mergeSort(fst p)) (mergeSort (snd p))

-- If a function can take multiple different inputs,  use type a and use pattern matching

-- Chapter 6
-- Higher order functions
doubleList::[Integer]->[Integer]
doubleList [] = []
doubleList (x:xs) = 2*x : doubleList xs

reverseList::[String]->[String]
reverseList [] = []
reverseList (x:xs) = reverse x : reverseList xs

-- Refactoring  apiece of code and making it a parameter
mapList::(a->b) ->[a]->[b]
mapList f [] = []
mapList f (x:xs) = f x :mapList f xs

filterOdds::[Integer]->[Integer]
filterOdds []=[]
filterOdds (x:xs)
    |odd x = x:filterOdds xs
    |otherwise = filterOdds xs

-- Creating Lambda notation (\x -> blah blah blah here)
-- It creates a function within the code
filterList::(a->Bool) -> [a]->[a]
filterList p [] = []
filterList p (x:xs)
    |p x = x :filterList p xs
    |otherwise = filterList p xs

