module School (School, add, empty, grade, sorted) where

import qualified Data.IntMap.Lazy as Map
import qualified Data.Set as Set

type School = Map.IntMap (Set.Set String)

add :: Int -> String -> School -> School
add gradeNum student school = error "You need to implement this function."

empty :: School
empty = Map.empty

grade :: Int -> School -> [String]
grade gradeNum school = error "You need to implement this function."

sorted :: School -> [(Int, [String])]
sorted = (fmap . fmap) Set.toAscList . Map.toAscList
