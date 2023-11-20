module Rumor.Parser.Clear
( clear
) where

import Rumor.Parser.Common (Parser, hlexeme, hlexeme1, hspace1, (<|>))

import qualified Data.Maybe as Maybe
import qualified Rumor.Internal as Rumor
import qualified Rumor.Parser.Identifier as Identifier
import qualified Text.Megaparsec as Mega

-- $setup
-- >>> import Rumor.Parser.Common
-- >>> import Rumor.Internal
-- >>> let parse inner = parseTest newContext (inner <* eof)

{-| Parses a clear command.

  >>> parse clear "clear"
  Clear ClearAll

  >>> parse clear "clear all"
  Clear ClearAll

  >>> parse clear "clear choice foobar"
  Clear (ClearChoice (Label (Unicode "foobar")))

  >>> parse clear "clear choices"
  Clear ClearChoices

  >>> parse clear "clear dialog"
  Clear ClearDialog

  You have to have at least one space, if you're using a clear type.

  >>> parse clear "clearall"
  1:6:
    |
  1 | clearall
    |      ^
  unexpected 'a'
  expecting "/*", end of input, or white space

  >>> parse clear "clearchoice foobar"
  1:6:
    |
  1 | clearchoice foobar
    |      ^
  unexpected 'c'
  expecting "/*", end of input, or white space

  >>> parse clear "clear choicefoobar"
  1:13:
    |
  1 | clear choicefoobar
    |             ^^
  unexpected "fo"
  expecting "/*" or white space

  >>> parse clear "clearchoices"
  1:6:
    |
  1 | clearchoices
    |      ^
  unexpected 'c'
  expecting "/*", end of input, or white space

  >>> parse clear "cleardialog"
  1:6:
    |
  1 | cleardialog
    |      ^
  unexpected 'd'
  expecting "/*", end of input, or white space

  You have to define a choice if you use the `choice` type

  >>> parse clear "clear choice"
  1:13:
    |
  1 | clear choice
    |             ^
  unexpected end of input
  expecting "/*" or white space

  >>> parse clear "clear choice "
  1:14:
    |
  1 | clear choice 
    |              ^
  unexpected end of input
  expecting identifier or white space
-}
clear :: Parser Rumor.Node
clear = do
  _ <- "clear"
  typ <- Mega.optional (hspace1 *> clearType)
  pure (Rumor.Clear (Maybe.fromMaybe Rumor.ClearAll typ))

clearType :: Parser Rumor.ClearType
clearType =
  let
    clearAll = do
      _ <- hlexeme "all"
      pure Rumor.ClearAll
    clearChoice = do
      _ <- hlexeme1 "choice"
      Rumor.ClearChoice . Rumor.Label <$> hlexeme Identifier.identifier
    clearChoices = do
      _ <- hlexeme "choices"
      pure Rumor.ClearChoices
    clearDialog = do
      _ <- hlexeme "dialog"
      pure Rumor.ClearDialog
  in
        Mega.try clearAll
    <|> Mega.try clearChoice
    <|> Mega.try clearChoices
    <|> clearDialog
