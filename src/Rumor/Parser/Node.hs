module Rumor.Parser.Node
( nodes
, node
) where

import Rumor.Parser.Common (Parser)

import Rumor.Parser.Action as Action
import Rumor.Parser.Control as Control
import Rumor.Parser.Dialog as Dialog
import Rumor.Parser.Lexeme as Lexeme
import Rumor.Internal.Types as Rumor
import Text.Megaparsec as Mega
import Text.Megaparsec.Char.Lexer as Lexer

nodes :: Parser [Rumor.Node]
nodes =
  Lexer.nonIndented Lexeme.space do
    Mega.manyTill (Lexeme.hlexeme node) (Mega.hidden Mega.eof)

node :: Parser Rumor.Node
node =
      Mega.try Dialog.say
  <|> Mega.try Dialog.add
  <|> Mega.try Action.action
  <|> Control.control node
