module DNA (nucleotideCounts) where

import qualified Data.Map as Map

nucleotides = "ACGT"
unknown = 'X'

zeros = Map.fromList [(nuc, 0) | nuc <- nucleotides]

nucleotideCounts :: String -> Either String (Map.Map Char Int)
nucleotideCounts = validate . count

count::String -> Map.Map Char Int
count xs = Map.fromListWith (+) pairs
    where pairs = [(if elem x nucleotides then x else unknown, 1) | x <- xs]


validate map
    | Map.member unknown map = Left "non-nucleotide"
    | otherwise = Right $ Map.union map zeros
