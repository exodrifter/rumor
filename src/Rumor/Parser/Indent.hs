module Rumor.Parser.Indent
( block
, indented
, sameOrIndented
, withPos
) where

import Rumor.Parser.Type (Parser(..))

import qualified Text.Parsec.Indent as Parsec

block :: Parser a -> Parser [a]
block = Parser . Parsec.block . unParser

indented :: Parser ()
indented = Parser Parsec.indented

sameOrIndented :: Parser ()
sameOrIndented = Parser Parsec.sameOrIndented

withPos :: Parser a -> Parser a
withPos = Parser . Parsec.withPos . unParser
