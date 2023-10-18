module Rumor.Parser.Choice
( choice
) where

import Rumor.Parser.Common (Parser)

import qualified Text.Megaparsec as Mega
import qualified Text.Megaparsec.Char as Char
import qualified Text.Megaparsec.Char.Lexer as Lexer
import qualified Rumor.Internal.Types as Rumor
import qualified Rumor.Parser.Surround as Surround
import qualified Rumor.Parser.Identifier as Identifier
import qualified Rumor.Parser.Lexeme as Lexeme
import qualified Rumor.Parser.Indented as Indented
import qualified Rumor.Parser.Unquoted as Unquoted

-- $setup
-- >>> import qualified Text.Megaparsec as Mega
-- >>> let parseTest inner = Mega.parseTest (inner <* Mega.hidden Mega.eof)

{-| Parses a choice, which has a label on the same line and an unquoted line
  afterwards. It can contain an indented block of nodes.

  With choice text on the same line:
  >>> import Rumor.Parser.Dialog (say)
  >>> parseTest (choice say) "choice [label]\n  > Choice A"
  Choice "label" (String "Choice A") Nothing

  With choice text on multiple lines:
  >>> import Rumor.Parser.Dialog (say)
  >>> parseTest (choice say) "choice [label]\n  > Choice\n    A"
  Choice "label" (Concat (String "Choice") (Concat (String " ") (String "A"))) Nothing

  With choice text on the next line:
  >>> import Rumor.Parser.Dialog (say)
  >>> parseTest (choice say) "choice [label]\n  >\n   Choice A"
  Choice "label" (String "Choice A") Nothing

  With indented nodes:
  >>> parseTest (choice say) "choice [label]\n  > Choice A\n  : Hello"
  Choice "label" (String "Choice A") (Just (Say Nothing (String "Hello") :| []))

  Error examples:
  >>> import Rumor.Parser.Dialog (say)
  >>> parseTest (choice say) "choice [label]\n  >\n  Choice A"
  3:10:
    |
  3 |   Choice A
    |          ^
  unexpected 'A'
  expecting ':'
-}
choice :: Parser Rumor.Node -> Parser Rumor.Node
choice inner = do
  originalRef <- Lexer.indentLevel
  _ <- Lexer.indentGuard Lexeme.space EQ originalRef

  _ <- Lexeme.hlexeme "choice"
  identifier <- Lexeme.hlexeme (Surround.brackets Identifier.identifier)
  _ <- Char.char '\n'

  indentedRef <- Lexer.indentGuard Lexeme.space GT originalRef
  choiceText <- Unquoted.unquotedBlock (Char.char '>') (const id)

  indentedNodes <- Mega.optional (Indented.someIndentedAt indentedRef inner)

  pure (Rumor.Choice identifier choiceText indentedNodes)
