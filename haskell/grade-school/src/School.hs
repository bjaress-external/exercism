module School (School, add, empty, grade, sorted) where

-- `containers` package
import qualified Data.IntMap.Lazy as Map
import qualified Data.Set as Set

import qualified Data.Maybe as Maybe


type School = Map.IntMap (Set.Set String)

add :: Int -> String -> School -> School
add = flip (Map.alter . inserter)
    where
    inserter student = Just . maybe (Set.singleton student) (Set.insert student)

empty :: School
empty = Map.empty

grade :: Int -> School -> [String]
grade = maybe [] Set.toAscList `from` Map.lookup
    where
    from = ((.).(.))

sorted :: School -> [(Int, [String])]
sorted = (fmap . fmap) Set.toAscList . Map.toAscList
