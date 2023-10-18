module Rumor.Parser.Identifier
( identifier
) where

import Data.Char (isLetter, isMark, isDigit)
import Data.NonEmptyText (NonEmptyText)
import Rumor.Parser.Common (Parser, (<?>))

import qualified Data.NonEmptyText as NET
import qualified Text.Megaparsec as Mega

-- $setup
-- >>> import qualified Text.Megaparsec as Mega
-- >>> let parseTest inner = Mega.parseTest (inner <* Mega.hidden Mega.eof)

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
    validChar ch =
         isLetter ch
      || isMark ch
      || isDigit ch
      || ch == '-'
      || ch == '_'

    label = "valid identifier character"
    parser = do
      first <- Mega.satisfy validChar <?> label
      rest <- Mega.takeWhileP (Just label) validChar
      pure (NET.new first rest)

  in
    parser <?> "identifier"
