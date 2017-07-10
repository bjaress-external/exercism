module RunLength (decode, encode) where

import qualified Data.List as List
import qualified Text.Parsec as Parsec
import qualified Text.Parsec.Char as PChar
import Control.Monad ((>=>))
import Control.Applicative ((<|>), (<$>))

type CharRun = (Int, Char)

decode :: String -> String
decode = runs >=> dec
    where
    runs :: String -> [CharRun]
    runs = either (const []) id . Parsec.parse parser ""

    parser :: Parsec.Parsec String st [CharRun]
    parser = Parsec.many $ numberedRun <|> singleton
    -- "123a" to (123, 'a')
    numberedRun = (,) . read <$> Parsec.many1 PChar.digit <*> PChar.anyChar
    -- "a" to (1, 'a')
    singleton = (,) 1 <$> PChar.anyChar

    -- (3, 'a') to "aaa"
    dec = uncurry replicate


encode :: String -> String
encode = runs >=> enc
    where
    runs :: String -> [CharRun]
    runs = fmap extract . List.group
    extract cs = (length cs, head cs)

    enc :: CharRun -> String
    enc (1, c) = [c]
    enc (n, c) = show n ++ [c]
