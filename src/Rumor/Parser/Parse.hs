module Rumor.Parser.Parse
( alphaNum
, anyChar
, char
, eol
, eof
, fixed
, oneOf
, restOfLine
, spaces
, string
) where

import Rumor.Parser.Combinator (many, manyTill)
import Rumor.Parser.Type (Parser(..))

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

eol :: Parser ()
eol = Parser $ void Parsec.endOfLine

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

-- | Consumes the rest of whitespace on this line, including the end of line
-- characters if they exist
restOfLine :: Parser ()
restOfLine = do
  _ <- manyTill space (eol <|> eof)
  eol <|> eof

sign :: HasResolution r => Parser (Fixed r -> Fixed r)
sign = Parser Parsec.sign

space :: Parser Char
space = Parser Parsec.space

spaces :: Parser ()
spaces = Parser Parsec.spaces

string :: T.Text -> Parser T.Text
string = Parser . fmap T.pack . Parsec.string . T.unpack
