module Rumor.Parser.Indent
( checkIndent
, checkIndented
, indented
, withPos
) where

import Rumor.Parser.Type (Parser(..))

import qualified Text.Parsec.Indent as Parsec

-- | Parses only when the current indentation level matches the reference
checkIndent :: Parser ()
checkIndent = Parser Parsec.checkIndent

-- | Parses only when the current indentation level matches the reference or is
-- indented past the reference
checkIndented :: Parser ()
checkIndented = checkIndent <|> indented

-- | Parses only when indented past the reference
indented :: Parser ()
indented = Parser Parsec.indented

withPos :: Parser a -> Parser a
withPos = Parser . Parsec.withPos . unParser
