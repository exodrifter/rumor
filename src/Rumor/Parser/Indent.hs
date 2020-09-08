module Rumor.Parser.Indent
( checkIndent
, checkIndented
, indented
, same
, withPos
) where

import Rumor.Parser.Type (Parser(..))

import qualified Text.Parsec.Indent as Parsec

-- | Parses only when the current indentation level matches the reference
checkIndent :: Parser r ()
checkIndent = Parser Parsec.checkIndent

-- | Parses only when the current indentation level matches the reference or is
-- indented past the reference
checkIndented :: Parser r ()
checkIndented = checkIndent <|> indented

-- | Parses only when indented past the reference
indented :: Parser r ()
indented = Parser Parsec.indented

-- | Parses only when on the same line as the reference
same :: Parser r ()
same = Parser Parsec.same

withPos :: Parser r a -> Parser r a
withPos = Parser . Parsec.withPos . unParser
