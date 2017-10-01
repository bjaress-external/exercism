module Hamming (distance) where

distance :: String -> String -> Maybe Int
distance xs ys
    | length xs /= length ys = Nothing
    | otherwise = Just $ countFalse $ zipWith (==) xs ys

countFalse = length . filter not
