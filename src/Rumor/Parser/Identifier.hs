module Rumor.Parser.Identifier
( identifier
, label
) where

import Data.Char (isLetter, isMark, isDigit)
import Data.NonEmptyText (NonEmptyText)
import Rumor.Parser.Common (Parser, (<?>))

import qualified Data.NonEmptyText as NET
import qualified Rumor.Internal.Types as Rumor
import qualified Rumor.Parser.Lexeme as Lexeme
import qualified Rumor.Parser.Surround as Surround
import qualified Text.Megaparsec as Mega
import qualified Text.Megaparsec.Char as Char

-- $setup
-- >>> import qualified Text.Megaparsec as Mega
-- >>> let parseTest inner = Mega.parseTest (inner <* Mega.eof)

{-| Parses a label, which is defined as any non-empty consecutive sequence of
  letters, digits, marks, underscores, and dashes surrounded by square brackets.

  >>> parseTest label "[alice]"
  Label "alice"

  >>> parseTest label "[alícia]"
  Label "al\237cia"

  >>> parseTest label "[アリス]"
  Label "\12450\12522\12473"

  >>> parseTest label "[123]"
  Label "123"

  >>> parseTest label "[alice-alícia-アリス]"
  Label "alice-al\237cia-\12450\12522\12473"

  >>> parseTest label "[alice_alícia_アリス]"
  Label "alice_al\237cia_\12450\12522\12473"

  You can have extra spaces between the brackets, but newlines are not okay.

  >>> parseTest label "[  alice  ]"
  Label "alice"

  >>> parseTest label "[\talice\t]"
  Label "alice"

  >>> parseTest label "[\nalice\n]"
  1:2:
    |
  1 | [
    |  ^
  unexpected newline
  expecting identifier

  Labels cannot be empty or contain spaces.

  >>> parseTest label "[]"
  1:2:
    |
  1 | []
    |  ^
  unexpected ']'
  expecting identifier

  >>> parseTest label "[alice alícia アリス]"
  1:8:
    |
  1 | [alice alícia アリス]
    |        ^
  unexpected 'a'
  expecting close bracket

  Trailing horizontal space is consumed, but not vertical space.

  >>> parseTest label "[alice]    "
  Label "alice"

  >>> parseTest label "[alice]\n"
  1:8:
    |
  1 | [alice]
    |        ^
  unexpected newline
  expecting end of input
-}
label :: Parser Rumor.Label
label =
  let
    parser =
      Surround.surround
        (Lexeme.hlexeme (Char.char '[') <?> "open bracket")
        (Lexeme.hlexeme (Char.char ']') <?> "close bracket")
        identifier
  in
    Rumor.Label <$> parser <?> "label"

{-| Parses an identifier, which is defined as any non-empty consecutive sequence
  of letters, digits, marks, underscores, and dashes.

  >>> parseTest identifier "alice"
  "alice"

  >>> parseTest identifier "alícia"
  "al\237cia"

  >>> parseTest identifier "アリス"
  "\12450\12522\12473"

  >>> parseTest identifier "123"
  "123"

  >>> parseTest identifier "alice-alícia-アリス"
  "alice-al\237cia-\12450\12522\12473"

  >>> parseTest identifier "alice_alícia_アリス"
  "alice_al\237cia_\12450\12522\12473"

  Identifiers cannot be empty or contain spaces.

  >>> parseTest identifier ""
  1:1:
    |
  1 | <empty line>
    | ^
  unexpected end of input
  expecting identifier

  >>> parseTest identifier "alice alícia アリス"
  1:7:
    |
  1 | alice alícia アリス
    |       ^
  unexpected 'a'
  expecting end of input

  Trailing horizontal space is consumed, but not vertical space.

  >>> parseTest identifier "alice    "
  "alice"

  >>> parseTest identifier "alice\n"
  1:6:
    |
  1 | alice
    |      ^
  unexpected newline
  expecting end of input or valid identifier character
-}
identifier :: Parser NonEmptyText
identifier =
  let
    validCharLabel = "valid identifier character"
    validChar ch =
         isLetter ch
      || isMark ch
      || isDigit ch
      || ch == '-'
      || ch == '_'

    parser = do
      first <- Mega.satisfy validChar <?> validCharLabel
      rest <- Mega.takeWhileP (Just validCharLabel) validChar
      pure (NET.new first rest)

  in
    Lexeme.hlexeme parser <?> "identifier"
