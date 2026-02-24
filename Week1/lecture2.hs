module Tutorial where

-- Function that computes the absolute value of a number
absolute :: Int -> Int -- Takes a single integer as an input and produces a single integer
absolute x = if x < 0 then -x else x

-- Prelude is the input to the Haskell interpreter. The next line
-- is the output
