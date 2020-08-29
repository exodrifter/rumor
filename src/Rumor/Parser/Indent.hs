module Rumor.Parser.Indent
( sameOrIndented
, withPos
) where

import Rumor.Parser.Type (Parser(..))

import qualified Text.Parsec.Indent as Parsec

sameOrIndented :: Parser ()
sameOrIndented = Parser Parsec.sameOrIndented

withPos :: Parser a -> Parser a
withPos = Parser . Parsec.withPos . unParser
