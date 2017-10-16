module DNA (nucleotideCounts) where

import qualified Data.Map as Map

nucleotides = "ACGT"
bad = 'X'

zeros = Map.fromList [(nuc, 0) | nuc <- nucleotides]

nucleotideCounts :: String -> Either String (Map.Map Char Int)
nucleotideCounts = validate . count

count::String -> Map.Map Char Int
count = Map.fromListWith (+) . pairs . fmap flagBad
    where
    pairs chars = [(c, 1) | c <- chars]
    flagBad char
        | elem char nucleotides = char
        | otherwise = bad


validate map
    | Map.member bad map = Left "non-nucleotide"
    | otherwise = Right $ Map.union map zeros
