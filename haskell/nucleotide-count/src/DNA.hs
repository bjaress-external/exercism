module DNA (nucleotideCounts) where

import qualified Data.Map.Strict as Map

nucleotides = "ACGT"

nucleotideCounts :: String -> Either String (Map.Map Char Int)
nucleotideCounts = validate . count

count::String -> Map.Map Char Int
count = Map.fromListWith (+) . (zeros ++) . fmap (has 1)
    where
    zeros = fmap (has 0) nucleotides
    has = flip (,)

validate totals
    | Map.size totals == length nucleotides = Right totals
    | otherwise = Left (show totals)
