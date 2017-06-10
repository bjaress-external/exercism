module Bob (responseFor) where

import Control.Applicative ((<|>))
import qualified Data.Maybe as Maybe
import qualified Data.Char as Char

responseFor :: String -> String
responseFor xs = Maybe.fromMaybe "Whatever." $
    asSilence trimmed <|> asShout trimmed <|> asQuestion trimmed
    where
    trimmed = filter (not . Char.isSpace) xs

asSilence [] = Just "Fine. Be that way!"
asSilence _ = Nothing

asQuestion = detect . reverse
    where
    detect ('?':xs) = Just "Sure."
    detect _ = Nothing

asShout chars =
    if someUpper chars && noLower chars
        then Just "Whoa, chill out!"
        else Nothing
    where
    someUpper = any Char.isUpper
    noLower = all (not . Char.isLower)
