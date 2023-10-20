module Rumor.Parser.Surround
( parentheses
, braces
, doubleQuotes
, surround
) where

import Rumor.Parser.Common (Parser, lexeme, (<?>))

import qualified Text.Megaparsec.Char as Char

-- $setup
-- >>> import Rumor.Parser.Common
-- >>> let parse inner = parseTest newContext (inner <* eof)

{-| Parses parentheses surrounding an inner parser. Any amount of space,
  including newlines, is allowed between the parentheses and the inner parser.

  Examples:
  >>> parse (parentheses "foobar") "(foobar)"
  "foobar"

  >>> parse (parentheses "foobar") "(  foobar  )"
  "foobar"

  >>> parse (parentheses "foobar") "(\nfoobar\n)"
  "foobar"

  >>> parse (parentheses "foobar") "(foobar"
  1:8:
    |
  1 | (foobar
    |        ^
  unexpected end of input
  expecting close parenthesis

  >>> parse (parentheses "foobar") "foobar)"
  1:1:
    |
  1 | foobar)
    | ^
  unexpected 'f'
  expecting open parenthesis

  >>> parse (parentheses "foobar") "()"
  1:2:
    |
  1 | ()
    |  ^
  unexpected ')'
  expecting "foobar"
-}
parentheses :: Parser a -> Parser a
parentheses inner =
  surround
    (lexeme (Char.char '(' <?> "open parenthesis"))
    (Char.char ')' <?> "close parenthesis")
    (lexeme inner)

{-| Parses braces surrounding an inner parser. Any amount of space,
  including newlines, is allowed between the braces and the inner parser.

  Examples:
  >>> parse (braces "foobar") "{foobar}"
  "foobar"

  >>> parse (braces "foobar") "{  foobar  }"
  "foobar"

  >>> parse (braces "foobar") "{\nfoobar\n}"
  "foobar"

  >>> parse (braces "foobar") "{foobar"
  1:8:
    |
  1 | {foobar
    |        ^
  unexpected end of input
  expecting close brace

  >>> parse (braces "foobar") "foobar}"
  1:1:
    |
  1 | foobar}
    | ^
  unexpected 'f'
  expecting open brace

  >>> parse (braces "foobar") "{}"
  1:2:
    |
  1 | {}
    |  ^
  unexpected '}'
  expecting "foobar"
-}
braces :: Parser a -> Parser a
braces inner =
  surround
    (lexeme (Char.char '{') <?> "open brace")
    (Char.char '}' <?> "close brace")
    (lexeme inner)

{-| Parses double quotes surrounding an inner parser. No space is allowed
  between the double quotes and the inner parser.

  Examples:
  >>> parse (doubleQuotes "foobar") "\"foobar\""
  "foobar"

  >>> parse (doubleQuotes "foobar") "\"  foobar  \""
  1:2:
    |
  1 | "  foobar  "
    |  ^^^^^^
  unexpected "  foob"
  expecting "foobar"

  >>> parse (doubleQuotes "foobar") "\"\nfoobar\n\""
  1:2:
    |
  1 | "
    |  ^
  unexpected "<newline>fooba"
  expecting "foobar"

  >>> parse (doubleQuotes "foobar") "\"foobar"
  1:8:
    |
  1 | "foobar
    |        ^
  unexpected end of input
  expecting close double quotes

  >>> parse (doubleQuotes "foobar") "foobar\""
  1:1:
    |
  1 | foobar"
    | ^
  unexpected 'f'
  expecting open double quotes

  >>> parse (doubleQuotes "foobar") "\"\""
  1:2:
    |
  1 | ""
    |  ^
  unexpected '"'
  expecting "foobar"
-}
doubleQuotes :: Parser a -> Parser a
doubleQuotes =
  surround
    (Char.char '"' <?> "open double quotes")
    (Char.char '"' <?> "close double quotes")

surround :: Parser a -> Parser b -> Parser c -> Parser c
surround begin end inner = do
  _ <- begin
  result <- inner
  _ <- end
  pure result
