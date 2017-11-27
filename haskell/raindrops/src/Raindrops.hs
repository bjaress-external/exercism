module Raindrops (convert) where

convert :: Int -> String
convert n
    | check n $ pling * plang * plong = "PlingPlangPlong"

    | check n $ pling * plang = "PlingPlang"
    | check n $ pling * plong = "PlingPlong"
    | check n $ plang * plong = "PlangPlong"

    | check n pling = "Pling"
    | check n plang = "Plang"
    | check n plong = "Plong"

    | otherwise = show n

    where
    pling = 3
    plang = 5
    plong = 7
    check n k = n `mod` k == 0
