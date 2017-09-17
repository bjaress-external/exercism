module Acronym (abbreviate) where

import qualified Text.Parsec as Parsec
import qualified Text.Parsec.Char as PChar
import Control.Applicative ((<|>), (<$>))
import Data.Char as Char


abbreviate :: String -> String
abbreviate = either (const []) id . Parsec.parse parser ""
    where
    parser :: Parsec.Parsec String st String
    parser = initials <$> Parsec.sepBy word noise

    noise = Parsec.many $ PChar.satisfy (not . Char.isAlpha)

    word = allLower <|> someHigher
    allLower = Parsec.many1 PChar.lower
    someHigher = concat <$> sequence [ Parsec.many1 PChar.upper, Parsec.many PChar.lower]

initials :: [String] -> String
initials = fmap (Char.toUpper . head)
