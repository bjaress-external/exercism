module DNA (toRNA) where
import qualified Data.List as List

toRNA :: String -> Maybe String
toRNA = sequence . fmap (flip List.lookup $ zip "ACGT" "UGCA")
