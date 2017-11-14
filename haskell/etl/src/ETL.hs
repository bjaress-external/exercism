module ETL (transform) where

import qualified Data.Map.Lazy as Map
import qualified Data.Char as Char
import qualified Data.Tuple as Tuple

transform :: Map.Map Int String -> Map.Map Char Int
transform = Map.fromList . fmap normalize . (>>= decompose) . Map.toList
    where

    normalize :: (Int, Char) -> (Char, Int)
    normalize = Tuple.swap . fmap Char.toLower

    decompose :: (Int, String) -> [(Int, Char)]
    decompose = uncurry $ fmap . (,)
