fact :: Integer -> Integer
fact n = if n <= 0 then 1 else n * fact(n-1)

f :: Double -> Double
f x = 3 * x^3 - 5 -- If to the power of pi, use **

g :: Double->Double->Double
g x y = x * y - y / x

h :: Double -> Double -> Double
h x y = if x <= y then x^2 else y^3

h' :: Double -> Double -> Double
h' x y | x <= y = x^2
       | otherwise = y^3

-- -------------------List vs Recursion-------------------------

sumUp :: Integer -> Integer
sumUp n = if n <= 0 then 0 else n + sumUp(n - 1)
-- With guards
sumUp' :: Integer -> Integer
sumUp' x | x <= 0 = 0
         | x >= 0 = x + sumUp'(x-1)
-- Pattern matching
sumUp'' :: Integer -> Integer
sumUp'' 0 = 0
sumUp'' x = x + sumUp''(x-1)

sumUp''' :: Integer -> Integer
sumUp''' x = sum[1..x]


-- Add up all numbers from 1 to x, that are divisible by 3
sumMod3 :: Integer -> Integer
sumMod3 x = if x <= 0 then 0 else if x `mod` 3 == 0 then x + sumMod3(x-1) else sumMod3(x-1)

sumMod3' :: Integer -> Integer
sumMod3' x | x <= 0 = 0
            | mod x 3 == 0 = x + sumMod3'(x-1)
            | otherwise = sumMod3'(x-1)

sumMod3'' :: Integer -> Integer
sumMod3'' x = sum[0, 3..x]

sumMod3''' :: Integer -> Integer
sumMod3''' x = sum[n | n <- [0..x], mod n 3 == 0]


sumMod3cubed :: Integer -> Integer
sumMod3cubed x
    |x <= 0 = 0
    | mod x 3 == 0 = x^3 + sumMod3cubed(x-1)
    | otherwise x = sumMod3Cubed(x-1)

sumModCubed' :: Integer -> Integer
sumModCubed' x = sum[n^3 | n <- [0..x], mod n 3 == 0]

cart2polar :: (Double, Double) -> (Double, Double)
cart2polar (x, y) =
    let r =  sqrt(x^2 + y^2)
        theta = atan (y/x)
    in (r, theta)
