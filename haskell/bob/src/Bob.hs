module Bob (responseFor) where

import qualified Data.Char as Char

responseFor :: String -> String
responseFor raw = if isSilence xs
    then "Fine. Be that way!"
    else
        if isShouting xs
        then "Whoa, chill out!"
        else
            if isQuestion xs
            then "Sure."
            else "Whatever."
    where
    xs = filter (not . Char.isSpace) raw

isSilence [] = True
isSilence _ = False

isQuestion = detect . reverse
    where
    detect ('?':xs) = True
    detect _ = False

isShouting chars = any Char.isUpper chars && all (not . Char.isLower) chars
