{-# LANGUAGE TupleSections #-}

module ETL (transform) where

import qualified Data.Map.Lazy as Map
import qualified Data.Char as Char

transform :: Map.Map Int String -> Map.Map Char Int
transform legacyData = Map.foldlWithKey accumulate Map.empty legacyData
    where
    accumulate :: Map.Map Char Int -> Int -> String -> Map.Map Char Int
    accumulate newTable points letters = Map.union newTable $ Map.fromList $ pairs points letters
    pairs :: Int -> String -> [(Char, Int)]
    pairs = fmap . flip ( (,) . Char.toLower )

