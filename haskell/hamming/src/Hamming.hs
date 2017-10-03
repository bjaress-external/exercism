module Hamming (distance) where

distance :: String -> String -> Maybe Int

distance (x:xs) (y:ys) = fmap (+diff) $ distance xs ys
    where
    diff = (if x == y then 0 else 1)
distance [] [] = Just 0
distance  _  _ = Nothing
