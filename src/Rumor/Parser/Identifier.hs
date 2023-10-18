module Rumor.Parser.Identifier
( identifier
, label
) where

import Data.Char (isLetter, isMark, isDigit)
import Data.NonEmptyText (NonEmptyText)
import Rumor.Parser.Common (Parser, (<?>))

import qualified Data.NonEmptyText as NET
import qualified Rumor.Internal.Types as Rumor
import qualified Rumor.Parser.Surround as Surround
import qualified Text.Megaparsec as Mega

-- $setup
-- >>> import qualified Text.Megaparsec as Mega
-- >>> let parseTest inner = Mega.parseTest (inner <* Mega.hidden Mega.eof)

{-| Parses a label, which is defined as any non-empty consecutive sequence
  of letters, digits, marks, underscores, and dashes surrounded by square
  brackets.

  Examples:
  >>> parseTest label "[alice]"
  Label "alice"

  >>> parseTest label "[alícia]"
  Label "al\237cia"

  >>> parseTest label "[アリス]"
  Label "\12450\12522\12473"

  >>> parseTest label "[123]" -- You can start with numbers!
  Label "123"

  >>> parseTest label "[alice-alícia-アリス]"
  Label "alice-al\237cia-\12450\12522\12473"

  >>> parseTest label "[alice_alícia_アリス]"
  Label "alice_al\237cia_\12450\12522\12473"

  >>> parseTest label "[  alice  ]" -- Extra spaces are okay
  Label "alice"

  >>> parseTest label "[\nalice\n]" -- Newlines are not okay
  1:2:
    |
  1 | [
    |  ^
  unexpected newline
  expecting identifier

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
-}
label :: Parser Rumor.Label
label = Rumor.Label <$> Surround.brackets identifier <?> "label"

{-| Parses an identifier, which is defined as any non-empty consecutive sequence
  of letters, digits, marks, underscores, and dashes.

  Examples:
  >>> parseTest identifier "alice"
  "alice"

  >>> parseTest identifier "alícia"
  "al\237cia"

  >>> parseTest identifier "アリス"
  "\12450\12522\12473"

  >>> parseTest identifier "123" -- You can start with numbers!
  "123"

  >>> parseTest identifier "alice-alícia-アリス"
  "alice-al\237cia-\12450\12522\12473"

  >>> parseTest identifier "alice_alícia_アリス"
  "alice_al\237cia_\12450\12522\12473"

  >>> parseTest identifier ""
  1:1:
    |
  1 | <empty line>
    | ^
  unexpected end of input
  expecting identifier

  >>> parseTest identifier "alice alícia アリス"
  1:6:
    |
  1 | alice alícia アリス
    |      ^
  unexpected space
  expecting valid identifier character
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
    parser <?> "identifier"
