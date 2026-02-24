-- Homework Assignment One - Jenna Mena

-- Question 1
multiplesOfSeventeen :: [Integer]
multiplesOfSeventeen = [x | x <- [1..200], mod x 17 == 0]

-- Question 2
radius :: Double-> Double-> Double
radius x y = sqrt(x^2 + y^2)

-- Question 3
sumEvens :: Integer-> Integer
sumEvens n = sum [x | x <- [1..n], even x]

-- Question 4
multiplyEnds :: [Integer] -> Integer
multiplyEnds m = if null m then 1 else head m * last m

-- Question 5
getLengths :: [String] -> [Int]
getLengths = map length

-- Question 6
dropLastTwo :: [Integer] -> [Integer]
dropLastTwo n = init(init n)

-- Question 7
findEmpty :: [String] -> Bool
findEmpty = any null

-- Question 8
checkPalindrome :: String -> Bool
checkPalindrome s = s == reverse s

-- Question 9
checkSize :: [Integer] -> Bool
checkSize l = length l >= 3 && head l >= 10

-- Question 10
checkAnySize :: Integer -> [Integer] -> Bool
checkAnySize a l = fromIntegral (length l) >=  a && head l >= a
