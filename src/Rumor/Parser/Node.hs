module Rumor.Parser.Node
( nodes
, node
) where

import Rumor.Parser.Common (Parser, hlexeme, space, (<|>))

import qualified Rumor.Parser.Action as Action
import qualified Rumor.Parser.Choice as Choice
import qualified Rumor.Parser.Control as Control
import qualified Rumor.Parser.Let as Let
import qualified Rumor.Parser.Dialog as Dialog
import qualified Rumor.Internal as Rumor
import qualified Text.Megaparsec as Mega
import qualified Text.Megaparsec.Char.Lexer as Lexer

nodes :: Parser [Rumor.Node]
nodes =
  Lexer.nonIndented space do
    _ <- Mega.many (hlexeme Let.let')
    Mega.manyTill (hlexeme node) (Mega.hidden Mega.eof)

node :: Parser Rumor.Node
node =
      Mega.try Dialog.say
  <|> Mega.try Dialog.add
  <|> Mega.try Action.action
  <|> Choice.choice node
  <|> Control.control node
