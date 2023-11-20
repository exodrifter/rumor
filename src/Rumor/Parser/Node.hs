module Rumor.Parser.Node
( nodes
, node
) where

import Rumor.Parser.Common (Parser, space, (<|>))

import qualified Rumor.Internal as Rumor
import qualified Rumor.Parser.Action as Action
import qualified Rumor.Parser.Choice as Choice
import qualified Rumor.Parser.Choose as Choose
import qualified Rumor.Parser.Clear as Clear
import qualified Rumor.Parser.Control as Control
import qualified Rumor.Parser.Dialog as Dialog
import qualified Rumor.Parser.Let as Let
import qualified Rumor.Parser.Set as Set
import qualified Text.Megaparsec as Mega
import qualified Text.Megaparsec.Char.Lexer as Lexer

nodes :: Parser [Rumor.Node]
nodes = do
  _ <- Mega.many (Mega.try (Lexer.nonIndented space Let.let'))
  Mega.manyTill (Lexer.nonIndented space node) (Mega.hidden Mega.eof)

node :: Parser Rumor.Node
node =
      Mega.try Dialog.say
  <|> Mega.try Dialog.add
  <|> Mega.try Action.action
  <|> Mega.try Set.set
  <|> Mega.try Clear.clear
  <|> Mega.try Choose.choose
  <|> Mega.try (Choice.choice node)
  <|> Control.control node
