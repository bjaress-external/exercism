module Raindrops (convert) where

convert :: Int -> String
convert n
    | pling n && plang n && plong n = "PlingPlangPlong"
    | pling n && plang n = "PlingPlang"
    | pling n && plong n = "PlingPlong"
    | plang n && plong n = "PlangPlong"
    | pling n = "Pling"
    | plang n = "Plang"
    | plong n = "Plong"
    | otherwise = show n

pling n = n `mod` 3 == 0
plang n = n `mod` 5 == 0
plong n = n `mod` 7 == 0
