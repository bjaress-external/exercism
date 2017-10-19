module School (School, add, empty, grade, sorted) where

-- `containers` package
import qualified Data.IntMap.Lazy as Map
import qualified Data.Set as Set

import qualified Data.Maybe as Maybe


type School = Map.IntMap (Set.Set String)


add :: Int -> String -> School -> School
add = Map.insertWith Set.union `wrap` Set.singleton
    where
    -- magic compose on second argument
    wrap = flip . ((.) .)

empty :: School
empty = Map.empty

grade :: Int -> School -> [String]
grade = Set.toAscList `from` Map.findWithDefault (Set.empty)
    where
    -- magic compose with two-arg function
    from = ((.).(.))

sorted :: School -> [(Int, [String])]
sorted = (fmap . fmap) Set.toAscList . Map.toAscList
