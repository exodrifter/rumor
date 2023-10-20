module Rumor.Parser.Identifier
( identifier
, label
) where

import Rumor.Parser.Common (Parser, hlexeme, (<?>))

import qualified Data.Char
import qualified Data.NonEmptyText as NET
import qualified Rumor.Internal.Types as Rumor
import qualified Rumor.Parser.Surround as Surround
import qualified Text.Megaparsec as Mega
import qualified Text.Megaparsec.Char as Char

-- $setup
-- >>> import qualified Text.Megaparsec as Mega
-- >>> let parseTest inner = Mega.parseTest (inner <* Mega.eof)

{-| Parses a label, which is defined as any non-empty consecutive sequence of
  letters, digits, marks, underscores, and dashes surrounded by square brackets.

  >>> parseTest label "[alice]"
  Label (Unicode "alice")

  >>> parseTest label "[alícia]"
  Label (Unicode "al\237cia")

  >>> parseTest label "[アリス]"
  Label (Unicode "\12450\12522\12473")

  >>> parseTest label "[123]"
  Label (Unicode "123")

  >>> parseTest label "[alice-alícia-アリス]"
  Label (Unicode "alice-al\237cia-\12450\12522\12473")

  >>> parseTest label "[alice_alícia_アリス]"
  Label (Unicode "alice_al\237cia_\12450\12522\12473")

  You can have extra spaces between the brackets, but newlines are not okay.

  >>> parseTest label "[  alice  ]"
  Label (Unicode "alice")

  >>> parseTest label "[\talice\t]"
  Label (Unicode "alice")

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
  Label (Unicode "alice")

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
        (hlexeme (Char.char '[') <?> "open bracket")
        (hlexeme (Char.char ']') <?> "close bracket")
        identifier
  in
    Rumor.Label <$> parser <?> "label"

{-| Parses an identifier, which is defined as any non-empty consecutive sequence
  of letters, digits, marks, underscores, and dashes.

  >>> parseTest identifier "alice"
  Unicode "alice"

  >>> parseTest identifier "alícia"
  Unicode "al\237cia"

  >>> parseTest identifier "アリス"
  Unicode "\12450\12522\12473"

  >>> parseTest identifier "123"
  Unicode "123"

  >>> parseTest identifier "alice-alícia-アリス"
  Unicode "alice-al\237cia-\12450\12522\12473"

  >>> parseTest identifier "alice_alícia_アリス"
  Unicode "alice_al\237cia_\12450\12522\12473"

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
  Unicode "alice"

  >>> parseTest identifier "alice\n"
  1:6:
    |
  1 | alice
    |      ^
  unexpected newline
  expecting end of input or identifier character

  Identifiers cannot start with mark characters.

  >>> parseTest identifier "◌̈mark"
  1:1:
    |
  1 | ◌̈mark
    | ^
  unexpected '◌'
  expecting identifier
-}
identifier :: Parser Rumor.Unicode
identifier =
  let
    validCharLabel = "identifier character"
    validStart ch =
         Data.Char.isLetter ch
      || Data.Char.isDigit ch
      || Data.Char.generalCategory ch == Data.Char.DashPunctuation
      || Data.Char.generalCategory ch == Data.Char.ConnectorPunctuation

    validEnd ch =
         validStart ch
      || Data.Char.isMark ch

    parser = do
      first <- Mega.satisfy validStart <?> validCharLabel
      rest <- Mega.takeWhileP (Just validCharLabel) validEnd
      pure (NET.new first rest)

  in
    Rumor.Unicode <$> hlexeme parser <?> "identifier"
