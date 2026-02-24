import GHC.Exts.Heap (GenClosure(value))
-- Recursion --

-- A One pass minimum bubble
minPass::Ord a=>[a]->[a]
minPass [] = []
minPass [x] = [x]
minPass (x:y:xs)
    |x >= y = y:minPass(x:xs)
    |otherwise = x:minPass(y:xs)

-- Count occurences
countElem::Eq a =>a->[a]->Int
countElem e (x:xs)
    |e == x     = 1 + countElem  e xs
    |otherwise = countElem e xs

-- Strings --

-- Does it start with a vowel?
startsWithVowel::String->Bool
startsWithVowel [] = False
startsWithVowel (x:_) = x `elem` "aeiouAEIOU"

-- If the first string is a prefix of the second, remove it
removePrefix::String->String->String
removePrefix _ [] = []
removePrefix [] s = s
removePrefix pre str
    |pre `isPrefixOf` str   = drop (length pre) str
    |otherwise = str

-- Return positions where a character appears
firstCharPositions::Char->String->[Int]
firstCharPositions e xs = helper 0 xs
    where
        helper _ [] = []
        helper n (x:xs)
            |e == x = n:helper(n+1) xs
            |otherwise = helper(n+1) xs

safeLookUp::Char->[(Char, Char)]->Char
safeLookUp  _ [] = '?'
safeLookUp x ((key, value):xs)
    |x == key   = value
    | otherwise = safeLookUp x xs