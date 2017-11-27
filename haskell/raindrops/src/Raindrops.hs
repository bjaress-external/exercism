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

pling = check 3
plang = check 5
plong = check 7

check k n = n `mod` k == 0
