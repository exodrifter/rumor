module Rumor.Parser.Identifier
( identifier
) where

import Data.Char (isLetter, isMark, isDigit)
import Data.Text (Text)
import Rumor.Parser.Common (Parser, (<?>))

import qualified Text.Megaparsec as Mega

-- $setup
-- >>> import qualified Text.Megaparsec as Mega
-- >>> let parseTest inner = Mega.parseTest (inner <* Mega.hidden Mega.eof)

{-| Parses an identifier, which is defined as any consecutive sequence of
  letters, digits, marks, underscores, and dashes.

  Examples:
  >>> parseTest identifier "alice"
  "alice"

  >>> parseTest identifier "アリス"
  "\12450\12522\12473"

  >>> parseTest identifier "alice-アリス"
  "alice-\12450\12522\12473"

  >>> parseTest identifier "alice_アリス"
  "alice_\12450\12522\12473"

  >>> parseTest identifier "alice アリス"
  1:6:
    |
  1 | alice アリス
    |      ^
  unexpected space
  expecting valid identifier character
-}
identifier :: Parser Text
identifier =
  let
    validChar ch =
         isLetter ch
      || isMark ch
      || isDigit ch
      || ch == '-'
      || ch == '_'
    ident =
      Mega.takeWhile1P
        (Just "valid identifier character")
        validChar
  in
    ident <?> "identifier"
