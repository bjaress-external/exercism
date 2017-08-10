module Squares (difference, squareOfSums, sumOfSquares) where

-- I just looked up some closed-form solutions.

difference :: Integral a => a -> a
difference n = (n^2 - 1) * (3*n^2 + 2*n) `quot` 12

squareOfSums :: Integral a => a -> a
squareOfSums n = (n * (n + 1) `quot` 2)^2

sumOfSquares :: Integral a => a -> a
sumOfSquares n = (2*n^3 + 3*n^2 + n) `quot` 6
