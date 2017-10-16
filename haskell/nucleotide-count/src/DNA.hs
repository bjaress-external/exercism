{-# LANGUAGE TupleSections #-}

module DNA (nucleotideCounts) where

import qualified Data.Map as Map

nucleotides = "ACGT"
bad = 'X'

nucleotideCounts :: String -> Either String (Map.Map Char Int)
nucleotideCounts = validate . count

count::String -> Map.Map Char Int
count = Map.fromListWith (+) . (zeros ++) . fmap (one . mergeBad)
    where
    zeros = fmap (,0) nucleotides
    one = (,1)
    mergeBad char
        | elem char nucleotides = char
        | otherwise = bad

validate totals
    | Map.member bad totals = Left (countBad totals)
    | otherwise = Right totals
    where
    countBad = show . Map.lookup bad


pairWith = flip (,)
