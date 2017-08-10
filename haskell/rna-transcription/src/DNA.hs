module DNA (toRNA) where
import qualified Data.List as List

toRNA :: String -> Maybe String
toRNA = sequence . fmap convert
    where
    convert :: Char -> Maybe Char
    convert = flip List.lookup $ zip "ACGT" "UGCA"
