module Rumor.Parser.Parse
( alphaNum
, anyChar
, char
, endOfLine
, eof
, fixed
, oneOf
, spaces
, string
) where

import Rumor.Parser.Combinator (many)
import Rumor.Parser.Type (Parser(..))

import Control.Monad (fail)
import qualified Data.Text as T
import qualified Text.Parsec as Parsec
import qualified Text.Parsec.Number as Parsec

alphaNum :: Parser Char
alphaNum = Parser Parsec.alphaNum

anyChar :: Parser Char
anyChar = Parser Parsec.anyChar

char :: Char -> Parser Char
char = Parser . Parsec.char

digit :: Parser Char
digit = Parser Parsec.digit

endOfLine :: Parser Char
endOfLine = Parser Parsec.endOfLine

eof :: Parser ()
eof = Parser Parsec.eof

fixed :: HasResolution r => Parser (Fixed r)
fixed = do
  s <- sign
  mn <- readMaybe <$> many (digit <|> char '.')
  case mn of
    Just n -> pure $ s n
    Nothing -> fail "failed to read fixed number"

oneOf :: [Char] -> Parser Char
oneOf = Parser . Parsec.oneOf

sign :: HasResolution r => Parser (Fixed r -> Fixed r)
sign = Parser Parsec.sign

spaces :: Parser ()
spaces = Parser Parsec.spaces

string :: T.Text -> Parser T.Text
string = Parser . fmap T.pack . Parsec.string . T.unpack
