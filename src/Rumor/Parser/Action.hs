module Rumor.Parser.Action
( action
) where

import Data.NonEmptyText (NonEmptyText)
import Rumor.Parser.Common (Parser, (<?>), (<|>))

import qualified Rumor.Internal.Types as Rumor
import qualified Rumor.Parser.Expression as Expression
import qualified Rumor.Parser.Lexeme as Lexeme
import qualified Rumor.Parser.Identifier as Identifier
import qualified Text.Megaparsec as Mega
import qualified Text.Megaparsec.Char as Char

-- $setup
-- >>> import qualified Text.Megaparsec as Mega
-- >>> let parseTest inner = Mega.parseTest (inner <* Mega.hidden Mega.eof)

{-| Parses an action with one to four string arguments.

  TODO: Support other argument types

  >>> parseTest action "foobar()"
  Action0 "foobar"

  >>> parseTest action "123()" -- Starting with numbers is okay
  Action0 "123"

  >>> parseTest action "foobar(\"1\")"
  Action1 "foobar" (String "1")

  >>> parseTest action "foobar(\"1\", \"2\")"
  Action2 "foobar" (String "1") (String "2")

  >>> parseTest action "foobar(\"1\", \"2\", \"3\")"
  Action3 "foobar" (String "1") (String "2") (String "3")

  >>> parseTest action "foobar(\"1\", \"2\", \"3\", \"4\")"
  Action4 "foobar" (String "1") (String "2") (String "3") (String "4")

  No spaces are okay:
  >>> parseTest action "foobar(\"1\",\"2\")"
  Action2 "foobar" (String "1") (String "2")

  >>> parseTest action "foobar(\"1\",\"2\",\"3\")"
  Action3 "foobar" (String "1") (String "2") (String "3")

  >>> parseTest action "foobar(\"1\",\"2\",\"3\",\"4\")"
  Action4 "foobar" (String "1") (String "2") (String "3") (String "4")

  Extra spaces are okay:
  >>> parseTest action "foobar  (  )"
  Action0 "foobar"

  >>> parseTest action "foobar  (  \"1\"  )"
  Action1 "foobar" (String "1")

  >>> parseTest action "foobar  (  \"1\"  ,  \"2\"  )"
  Action2 "foobar" (String "1") (String "2")

  >>> parseTest action "foobar  (  \"1\"  ,  \"2\"  ,  \"3\"  )"
  Action3 "foobar" (String "1") (String "2") (String "3")

  >>> parseTest action "foobar  (  \"1\"  ,  \"2\"  ,  \"3\"  ,  \"4\"  )"
  Action4 "foobar" (String "1") (String "2") (String "3") (String "4")

  Extra newlines between the arguments are okay:
  >>> parseTest action "foobar  (\n)"
  Action0 "foobar"

  >>> parseTest action "foobar  (\n\"1\"\n)"
  Action1 "foobar" (String "1")

  >>> parseTest action "foobar  (\n\"1\"\n,\n\"2\"\n)"
  Action2 "foobar" (String "1") (String "2")

  >>> parseTest action "foobar  (\n\"1\"\n,\n\"2\"\n,\n\"3\"\n)"
  Action3 "foobar" (String "1") (String "2") (String "3")

  >>> parseTest action "foobar  (\n\"1\"\n,\n\"2\"\n,\n\"3\"\n,\n\"4\"\n)"
  Action4 "foobar" (String "1") (String "2") (String "3") (String "4")
-}
action :: Parser Rumor.Node
action = do
  actionName <- Lexeme.hlexeme Identifier.identifier
  _ <- Lexeme.lexeme (Char.char '(')

  result <-
        Mega.try (Lexeme.lexeme (action4 actionName))
    <|> Mega.try (Lexeme.lexeme (action3 actionName))
    <|> Mega.try (Lexeme.lexeme (action2 actionName))
    <|> Mega.try (Lexeme.lexeme (action1 actionName))
    <|> pure (Rumor.Action0 actionName)

  _ <- Char.char ')' <?> "end parentheses"
  pure result

action1 :: NonEmptyText -> Parser Rumor.Node
action1 actionName = do
  param1 <- Expression.stringExpression
  pure (Rumor.Action1 actionName param1)

action2 :: NonEmptyText -> Parser Rumor.Node
action2 actionName = do
  param1 <- Lexeme.lexeme Expression.stringExpression
  _ <- Lexeme.lexeme (Char.char ',')
  param2 <- Expression.stringExpression
  pure (Rumor.Action2 actionName param1 param2)

action3 :: NonEmptyText -> Parser Rumor.Node
action3 actionName = do
  param1 <- Lexeme.lexeme Expression.stringExpression
  _ <- Lexeme.lexeme (Char.char ',')
  param2 <- Lexeme.lexeme Expression.stringExpression
  _ <- Lexeme.lexeme (Char.char ',')
  param3 <- Expression.stringExpression
  pure (Rumor.Action3 actionName param1 param2 param3)

action4 :: NonEmptyText -> Parser Rumor.Node
action4 actionName = do
  param1 <- Lexeme.lexeme Expression.stringExpression
  _ <- Lexeme.lexeme (Char.char ',')
  param2 <- Lexeme.lexeme Expression.stringExpression
  _ <- Lexeme.lexeme (Char.char ',')
  param3 <- Lexeme.lexeme Expression.stringExpression
  _ <- Lexeme.lexeme (Char.char ',')
  param4 <- Expression.stringExpression
  pure (Rumor.Action4 actionName param1 param2 param3 param4)
