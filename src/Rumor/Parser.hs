module Rumor.Parser
( Parser
, Parsec.ParseError
, runParser

-- Combinators
, chainl1
, many
, many1
, manyTill

-- Indents
, sameOrIndented
, withPos

-- Parsers
, alphaNum
, anyChar
, char
, endOfLine
, eof
, fixed
, oneOf
, spaces
, string
) where

import Control.Applicative (Applicative(..), Alternative(empty))
import Control.Monad (Monad(..), MonadFail(..))
import qualified Data.Text as T
import qualified Text.Parsec as Parsec
import qualified Text.Parsec.Number as Parsec
import qualified Text.Parsec.Indent as Parsec

-- A parser that automatically rolls back when it fails, like
-- attoparsec.
newtype Parser a =
  Parser
    { unParser :: Parsec.IndentParser T.Text () a
    }

instance Functor Parser where
  fmap fn p = Parser $ fmap fn (unParser p)

instance Applicative Parser where
  pure a = Parser $ pure a
  l <*> r = Parser $ unParser l <*> unParser r

instance Alternative Parser where
  empty = Parser empty
  l <|> r = Parser $ Parsec.try (unParser l) <|> Parsec.try (unParser r)

instance Monad Parser where
  return = pure
  p >>= f = Parser $ unParser p >>= (unParser . f)

instance MonadFail Parser where
  fail = Parser . Parsec.parserFail

runParser :: Parser a -> Parsec.SourceName -> T.Text -> Either Parsec.ParseError a
runParser p = Parsec.runIndentParser (unParser p) ()

--------------------------------------------------------------------------------
-- Combinators
--------------------------------------------------------------------------------

chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainl1 p op = Parser $ Parsec.chainl1 (unParser p) (unParser op)

many :: Parser a -> Parser [a]
many = Parser . Parsec.many . unParser

many1 :: Parser a -> Parser [a]
many1 = Parser . Parsec.many1 . unParser

manyTill :: Parser a -> Parser end -> Parser [a]
manyTill p end =
  Parser $ Parsec.manyTill
    (unParser p)
    (Parsec.try . Parsec.lookAhead $ unParser end)

--------------------------------------------------------------------------------
-- Indents
--------------------------------------------------------------------------------

sameOrIndented :: Parser ()
sameOrIndented = Parser Parsec.sameOrIndented

withPos :: Parser a -> Parser a
withPos = Parser . Parsec.withPos . unParser

--------------------------------------------------------------------------------
-- Parsers
--------------------------------------------------------------------------------

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
