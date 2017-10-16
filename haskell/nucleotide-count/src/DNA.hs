module DNA (nucleotideCounts) where

import qualified Data.Map.Strict as Map

nucleotides = "ACGT"
bad = 'X'

nucleotideCounts :: String -> Either String (Map.Map Char Int)
nucleotideCounts = validate . count

count::String -> Map.Map Char Int
count = Map.fromListWith (+) . (zeros ++) . fmap one
    where
    zeros = fmap (has 0) nucleotides
    one char
        | elem char nucleotides = has 1 char
        | otherwise = has 1 bad
    has = flip (,)

validate totals
    | Map.member bad totals = Left (show totals)
    | otherwise = Right totals
