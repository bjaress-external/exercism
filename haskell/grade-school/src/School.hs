module School (School, add, empty, grade, sorted) where

import qualified Data.IntMap.Lazy as Map
import qualified Data.Set as Set
import qualified Data.Maybe as Maybe

type School = Map.IntMap (Set.Set String)

add :: Int -> String -> School -> School
add gradeNum student school
    | Map.member gradeNum school =
        Map.adjust (Set.insert student) gradeNum school
    | otherwise =
        Map.insert gradeNum (Set.singleton student) school

empty :: School
empty = Map.empty

grade :: Int -> School -> [String]
grade gradeNum school = maybe [] Set.toAscList $ Map.lookup gradeNum school

sorted :: School -> [(Int, [String])]
sorted = (fmap . fmap) Set.toAscList . Map.toAscList
