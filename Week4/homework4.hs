-- Standard Higher-Order Functions

-- -----------Section 2.1-----------
mapPair::(a -> b -> c) -> [(a, b)] -> [c]
mapPair f = map (uncurry f) 

mapPair'::(a -> b -> c) -> [(b, a)] -> [c]
mapPair' f = map(uncurry(flip f))


-- -----------Section 2.2 filter-----------
digitsOnly:: [Int] -> [Int]
digitsOnly = filter (\x -> x>=0 && x<= 9)

removeXs::[String]->[String]
removeXs = filter(\x -> null x || head x /= 'X')


-- -----------Section 2.3 map-------------
sqLens::[String]->[Integer]
sqLens = map(\s -> (fromIntegral (length s))^2)

bang::[String]->[String]
bang = map ( ++ "!")


-- -----------Section 2.4 zipWith-----------
diff::[Integer]->[Integer]->[Integer]
diff = zipWith (-)

splice::[String]->[String]->[String]
splice = zipWith (\x y -> x ++ y ++ x)


-- -----------Section 2.5 takeWhile-----------
firstStop::String->String
firstStop = takeWhile (/= '.')

boundRange::Integer->[Integer]->[Integer]
boundRange n = takeWhile (\x -> x >= -n && x <= n) 



-- List Recursion via Folds

-- -- Question 1 --
exists::(a->Bool)->[a]->Bool
exists _ [] = False
exists p (x:xs)
    |p x    = True
    |otherwise  = exists p xs

exists'::(a->Bool)->[a]->Bool
exists' p = foldr (\x y -> p x || y) False

-- -- Question 2 --
noDups::Eq a=>[a]->[a]
noDups [] = []
noDups (x:xs) = x : noDups(filter (/= x) xs)

noDups'::Eq a=>[a]->[a]
noDups' = foldr (\x y-> if x `elem` y then y else x:y) []

-- -- Question 3 --
countOverflow::Integer->[String]->Integer
countOverflow _ [] = 0
countOverflow n (x:xs)
    |toInteger(length x) > n    = 1+countOverflow n xs
    |otherwise                  = countOverflow n xs

countOverflow'::Integer->[String]->Integer
countOverflow' n = foldr (\x y -> if toInteger(length x) > n
        then y + 1 else y) 0

-- -- Question 4 --
concatList::[[a]]->[a]
concatList [] = []
concatList (x:xs) = x ++ concatList xs

concatList'::[[a]]->[a]
concatList' = foldr (++) []

-- -- Question 5 --
bindList::(a->[b])->[a]->[b]
bindList _ [] = []
bindList e (x:xs) = e x ++ bindList e xs

bindList'::(a->[b])->[a]->[b]
bindList' e = foldr (\x y-> e x ++ y) []
