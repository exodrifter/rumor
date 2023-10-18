module Rumor.Parser.Indented
( someIndentedMoreThan
, someIndentedAt
) where

import Rumor.Parser.Common (Parser)
import Data.List.NonEmpty (NonEmpty(..))

import qualified Text.Megaparsec as Mega
import qualified Text.Megaparsec.Char.Lexer as Lexer
import qualified Rumor.Parser.Lexeme as Lexeme

-- $setup
-- >>> import qualified Text.Megaparsec as Mega
-- >>> let parseTest inner = Mega.parseTest (inner <* Mega.hidden Mega.eof)

{-| Parse a non-empty block that has a greater indentation level than the
  reference position.

  Examples:
  >>> let parse pos = parseTest (someIndentedMoreThan (Mega.mkPos pos) "foo")
  >>> parse 1 "  foo"
  "foo" :| []

  >>> parse 1 "  foo\n  foo\n  foo"
  "foo" :| ["foo","foo"]

  >>> parse 3 "  foo\n  foo\n  foo"
  1:3:
    |
  1 |   foo
    |   ^
  incorrect indentation (got 3, should be greater than 3)
-}
someIndentedMoreThan :: Mega.Pos -> Parser a -> Parser (NonEmpty a)
someIndentedMoreThan originalRef inner = do
  ref <- Lexer.indentGuard Lexeme.space GT originalRef
  someIndentedAt ref inner

{-| Parse a non-empty block that has the same indentation level as the reference
  position.

  Examples:
  >>> let parse pos = parseTest (someIndentedAt (Mega.mkPos pos) "foo")
  >>> parse 1 "foo"
  "foo" :| []

  >>> parse 1 "  foo\n  foo\n  foo"
  1:3:
    |
  1 |   foo
    |   ^
  incorrect indentation (got 3, should be equal to 1)
-}
someIndentedAt :: Mega.Pos -> Parser a -> Parser (NonEmpty a)
someIndentedAt ref inner = do
  let
    indentedNode = do
      _ <- Lexer.indentGuard Lexeme.space EQ ref
      inner
  first <- indentedNode
  rest <- Mega.many indentedNode
  pure (first :| rest)
