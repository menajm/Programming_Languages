import System.Win32 (xBUTTON1)
k::Double->Double
k x = x^2 + 3*x - 5

m::Double->Double->Double
m x y = sqrt(x^2 + y^2)

signVal::Double->Integer
signVal x = if x > 0 then 1
     else if x == 0 then 0
     else -1

-- Guard practice
signVal'::Double->Integer
signVal' x
    |x > 0 = 1
    |x == 0 = 0
    |otherwise = -1

scoreBonus::Integer->Integer
scoreBonus x
    |x < 50 = x
    |x >= 50 && x < 80 = x + 10
    |otherwise = x + 20

-- List Comprehension & Ranges
multiplesOfSix::[Integer]
multiplesOfSix = [x | x <-[1..150], x `mod` 6 == 0]

oddNum::[Integer]
oddNum = [20..60]

squares::[Integer]
squares = [x^2| x<-[1..10]]
-- or squares = map (^2) [1..10]

-- Recursion vs. Built-in's
sumCubes::Integer->Integer
sumCubes c = sum [x^3 | x<-[0..c]]
sumCubes'::Integer->Integer
sumCubes' c = if c == 0 then c else c^3 + sumCubes'(c-1)

sumEvenSquares::Integer->Integer
sumEvenSquares s = sum [x^2|x<-[0..s], even x]

sumEvenSquares'::Integer->Integer
sumEvenSquares' n = if n == 0 then n else if even n then n^2 + sumEvenSquares'(n-1) else sumEvenSquares'(n-1)

countDown::Integer->Integer
countDown n = if n == 0 then 1 else 1 + countDown(n-1)

-- ---------------List Manipulation------------------------
getThird::[Integer]->Integer
getThird t = (take 3 t) !! 2

removeFirst::[a]->[a]
removeFirst = tail

endsMatch::[Integer]->Bool
endsMatch x = head x == last x

containsZero :: [Integer] -> Bool
containsZero = elem 0

-- ---------------String Based Problems--------------------
lastChar :: String -> Char
lastChar = last

isShort :: String -> Bool
isShort s = length s < 5

mirror :: String -> String
mirror m = m ++ reverse m

checkAtLeast :: Integer -> [Integer] -> Bool
checkAtLeast c l = length l >= fromIntegral c && head l >= c

checkThree :: [Integer] -> Bool
checkThree l = length l == 3 && last l < 0

-------------------------------------------------------------
-- Write a function that returns all numbers from 1 to 100
-- divisible by 5
div5::[Integer]
div5 = [x | x<-[1..100], x `mod` 5 == 0]

-- Write a function that returns True if a list has at least
-- 2 elements
atLeastTwo::[Integer] -> Bool
atLeastTwo l = length l >= 2

--Multiply the first and last element of a list
mulBigEnd::[Integer]->Integer
mulBigEnd l = head l * last l