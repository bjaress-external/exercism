module SumOfMultiples (sumOfMultiples) where

sumOfMultiples :: [Integer] -> Integer -> Integer
sumOfMultiples factors limit = sum $ filter isMultiple [1..(limit-1)]
    where
    isMultiple n = any (factorOf n) factors
    factorOf n = (0 ==) . mod n
