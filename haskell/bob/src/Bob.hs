module Bob (responseFor) where

import Control.Applicative ((<|>))
import qualified Data.Maybe as Maybe
import qualified Data.Char as Char

responseFor :: String -> String
responseFor chars = Maybe.fromMaybe "Whatever." $
    asSilence nub <|> asShout nub <|> asQuestion nub
    where
    -- strip out all spaces
    nub = filter (not . Char.isSpace) chars

asSilence [] = Just "Fine. Be that way!"
asSilence _ = Nothing

asShout chars =
    if someUpper chars && noLower chars
        then Just "Whoa, chill out!"
        else Nothing
    where
    someUpper = any Char.isUpper
    noLower = all (not . Char.isLower)

asQuestion chars = if length chars > 0 && (last chars) == '?'
    then Just "Sure."
    else Nothing
