module DNA (nucleotideCounts) where

import qualified Data.Map as Map

nucleotides = "ACGT"
bad = 'X'

nucleotideCounts :: String -> Either String (Map.Map Char Int)
nucleotideCounts = validate . count

count::String -> Map.Map Char Int
count = Map.fromListWith (+) . fmap (increment . flagBad)
    where
    increment = pairWith 1
    flagBad char
        | elem char nucleotides = char
        | otherwise = bad

validate totals
    | Map.member bad totals = Left $ countBad totals
    | otherwise = (Right . Map.union totals . zeros) nucleotides
    where
    countBad = show . Map.lookup bad
    normalize :: Map.Map Char Int -> Map.Map Char Int
    normalize = flip Map.union $ zeros nucleotides
    zeros = Map.fromList . fmap (pairWith 0)

pairWith = flip (,)
