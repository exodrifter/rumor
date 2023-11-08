module Rumor.Parser.Identifier
( identifier
, label
, variableName
) where

import Rumor.Parser.Common (Parser, hlexeme, rumorError, (<?>))

import qualified Data.Char
import qualified Data.NonEmptyText as NET
import qualified Rumor.Internal as Rumor
import qualified Rumor.Parser.Surround as Surround
import qualified Text.Megaparsec as Mega
import qualified Text.Megaparsec.Char as Char

-- $setup
-- >>> import Rumor.Parser.Common
-- >>> let parse inner = parseTest newContext (inner <* eof)

{-| Parses a label, which is defined as any non-empty consecutive sequence of
  letters, digits, marks, underscores, and dashes surrounded by square brackets.

  >>> parse label "[alice]"
  Label (Unicode "alice")

  >>> parse label "[alícia]"
  Label (Unicode "al\237cia")

  >>> parse label "[アリス]"
  Label (Unicode "\12450\12522\12473")

  >>> parse label "[123]"
  Label (Unicode "123")

  >>> parse label "[alice-alícia-アリス]"
  Label (Unicode "alice-al\237cia-\12450\12522\12473")

  >>> parse label "[alice_alícia_アリス]"
  Label (Unicode "alice_al\237cia_\12450\12522\12473")

  You can have extra spaces between the brackets, but newlines are not okay.

  >>> parse label "[  alice  ]"
  Label (Unicode "alice")

  >>> parse label "[\talice\t]"
  Label (Unicode "alice")

  >>> parse label "[\nalice\n]"
  1:2:
    |
  1 | [
    |  ^
  unexpected newline
  expecting identifier

  Labels cannot be empty or contain spaces.

  >>> parse label "[]"
  1:2:
    |
  1 | []
    |  ^
  unexpected ']'
  expecting identifier

  >>> parse label "[alice alícia アリス]"
  1:8:
    |
  1 | [alice alícia アリス]
    |        ^
  unexpected 'a'
  expecting close bracket

  Trailing horizontal space is consumed, but not vertical space.

  >>> parse label "[alice]    "
  Label (Unicode "alice")

  >>> parse label "[alice]\n"
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

  >>> parse identifier "alice"
  Unicode "alice"

  >>> parse identifier "alícia"
  Unicode "al\237cia"

  >>> parse identifier "アリス"
  Unicode "\12450\12522\12473"

  >>> parse identifier "123"
  Unicode "123"

  >>> parse identifier "alice-alícia-アリス"
  Unicode "alice-al\237cia-\12450\12522\12473"

  >>> parse identifier "alice_alícia_アリス"
  Unicode "alice_al\237cia_\12450\12522\12473"

  Identifiers cannot be empty or contain spaces.

  >>> parse identifier ""
  1:1:
    |
  1 | <empty line>
    | ^
  unexpected end of input
  expecting identifier

  >>> parse identifier "alice alícia アリス"
  1:7:
    |
  1 | alice alícia アリス
    |       ^
  unexpected 'a'
  expecting end of input

  Trailing horizontal space is consumed, but not vertical space.

  >>> parse identifier "alice    "
  Unicode "alice"

  >>> parse identifier "alice\n"
  1:6:
    |
  1 | alice
    |      ^
  unexpected newline
  expecting end of input or identifier character

  Identifiers cannot start with mark characters.

  >>> parse identifier "◌̈mark"
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

{-| Parses the name of a variable. A variable name is any identifier that
  doesn't start with a number and isn't a reserved keyword.

  >>> parse variableName "foobar"
  VariableName (Unicode "foobar")

  >>> parse variableName "foo123"
  VariableName (Unicode "foo123")

  >>> parse variableName "123foo"
  1:1:
    |
  1 | 123foo
    | ^
  unexpected '1'
  expecting variable name

  >>> parse variableName "true"
  1:1:
    |
  1 | true
    | ^^^^
  Cannot use true as a variable name
-}
variableName :: Parser Rumor.VariableName
variableName =
  let
    validCharLabel = "variable character"
    validStart ch =
         Data.Char.isLetter ch
      || Data.Char.generalCategory ch == Data.Char.DashPunctuation
      || Data.Char.generalCategory ch == Data.Char.ConnectorPunctuation
    validEnd ch =
         validStart ch
      || Data.Char.isDigit ch
      || Data.Char.isMark ch

    parser = do
      first <- Mega.satisfy validStart <?> validCharLabel
      rest <- Mega.takeWhileP (Just validCharLabel) validEnd
      pure (NET.new first rest)

    reservedKeywords =
      [ NET.new 't' "rue"
      , NET.new 'f' "alse"
      , NET.new 'n' "ot"
      , NET.new 'a' "nd"
      , NET.new 'o' "r"
      , NET.new 'x' "or"
      , NET.new 'i' "s"
      ]

  in do
    pos <- Mega.getOffset
    name <- parser <?> "variable name"
    if name `elem` reservedKeywords
    then rumorError
            ("Cannot use " <> NET.toText name <> " as a variable name")
            pos
            (NET.length name)
    else
      pure (Rumor.VariableName (Rumor.Unicode name))
