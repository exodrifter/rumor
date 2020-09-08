module Rumor.Parser.Parse
( alphaNum
, anyChar
, char
, eol
, eof
, fixed
, oneOf
, restOfFile
, restOfLine
, spaces
, spaces1
, string
) where

import Rumor.Parser.Combinator
import Rumor.Parser.Type (Parser(..))

import qualified Data.Text as T
import qualified Text.Parsec as Parsec
import qualified Text.Parsec.Number as Parsec

alphaNum :: Parser r Char
alphaNum = Parser Parsec.alphaNum

anyChar :: Parser r Char
anyChar = Parser Parsec.anyChar

char :: Char -> Parser r Char
char = Parser . Parsec.char

digit :: Parser r Char
digit = Parser Parsec.digit

eol :: Parser r ()
eol = Parser $ void Parsec.endOfLine

eof :: Parser r ()
eof = Parser Parsec.eof

fixed :: HasResolution r => Parser r (Fixed r)
fixed = do
  s <- sign
  mn <- readMaybe <$> many (digit <|> char '.')
  case mn of
    Just n -> pure $ s n
    Nothing -> fail "failed to read fixed number"

oneOf :: [Char] -> Parser r Char
oneOf = Parser . Parsec.oneOf

-- | Consumes the rest of whitespace in this document
restOfFile :: Parser r ()
restOfFile = do
  _ <- manyTill space eof
  eof

-- | Consumes the rest of whitespace on this line, including the end of line
-- characters if they exist
restOfLine :: Parser r ()
restOfLine = do
  _ <- manyTill space (eol <|> eof)
  eol <|> eof

sign :: HasResolution r => Parser r (Fixed r -> Fixed r)
sign = Parser Parsec.sign

space :: Parser r Char
space = Parser Parsec.space

spaces :: Parser r ()
spaces = Parser Parsec.spaces

spaces1 :: Parser r ()
spaces1 = Parser (Parsec.space *> Parsec.spaces)

string :: T.Text -> Parser r T.Text
string = Parser . fmap T.pack . Parsec.string . T.unpack
