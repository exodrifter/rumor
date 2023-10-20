module Rumor.Parser.Action
( action
) where

import Data.NonEmptyText (NonEmptyText)
import Rumor.Parser.Common (Parser, hlexeme, lexeme, (<?>), (<|>))

import qualified Rumor.Internal.Types as Rumor
import qualified Rumor.Parser.Expression as Expression
import qualified Rumor.Parser.Identifier as Identifier
import qualified Text.Megaparsec as Mega
import qualified Text.Megaparsec.Char as Char

-- $setup
-- >>> import qualified Text.Megaparsec as Mega
-- >>> let parseTest inner = Mega.parseTest (inner <* Mega.eof)

{-| Parses an action with one to four string arguments.

  TODO: Support other argument types

  An action is an identifier followed by a set of parenthesis containing zero to
  four comma-separated arguments.

  >>> parseTest action "foobar()"
  Action0 "foobar"

  >>> parseTest action "123()"
  Action0 "123"

  >>> parseTest action "foobar(\"1\")"
  Action1 "foobar" (String "1")

  >>> parseTest action "foobar(\"1\", \"2\")"
  Action2 "foobar" (String "1") (String "2")

  >>> parseTest action "foobar(\"1\", \"2\", \"3\")"
  Action3 "foobar" (String "1") (String "2") (String "3")

  >>> parseTest action "foobar(\"1\", \"2\", \"3\", \"4\")"
  Action4 "foobar" (String "1") (String "2") (String "3") (String "4")

  No spaces are okay.

  >>> parseTest action "foobar(\"1\",\"2\")"
  Action2 "foobar" (String "1") (String "2")

  >>> parseTest action "foobar(\"1\",\"2\",\"3\")"
  Action3 "foobar" (String "1") (String "2") (String "3")

  >>> parseTest action "foobar(\"1\",\"2\",\"3\",\"4\")"
  Action4 "foobar" (String "1") (String "2") (String "3") (String "4")

  Extra spaces are okay.

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

  Extra newlines are okay.

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

  Both parenthesis must be provided.
  >>> parseTest action "foobar("
  1:8:
    |
  1 | foobar(
    |        ^
  unexpected end of input
  expecting close parenthesis or open double quotes

  >>> parseTest action "foobar)"
  1:7:
    |
  1 | foobar)
    |       ^
  unexpected ')'
  expecting open parenthesis or valid identifier character

  Trailing whitespace is consumed.

  >>> parseTest action "foobar()  "
  Action0 "foobar"

  >>> parseTest action "foobar()  \n"
  Action0 "foobar"

  >>> parseTest action "foobar()  \n  "
  Action0 "foobar"
-}
action :: Parser Rumor.Node
action = do
  actionName <- hlexeme Identifier.identifier
  _ <- lexeme (Char.char '(') <?> "open parenthesis"

  result <-
        Mega.try (lexeme (action4 actionName))
    <|> Mega.try (lexeme (action3 actionName))
    <|> Mega.try (lexeme (action2 actionName))
    <|> Mega.try (lexeme (action1 actionName))
    <|> pure (Rumor.Action0 actionName)

  _ <- lexeme (Char.char ')') <?> "close parenthesis"
  pure result

action1 :: NonEmptyText -> Parser Rumor.Node
action1 actionName = do
  param1 <- Expression.stringExpression
  pure (Rumor.Action1 actionName param1)

action2 :: NonEmptyText -> Parser Rumor.Node
action2 actionName = do
  param1 <- lexeme Expression.stringExpression
  _ <- lexeme (Char.char ',')
  param2 <- Expression.stringExpression
  pure (Rumor.Action2 actionName param1 param2)

action3 :: NonEmptyText -> Parser Rumor.Node
action3 actionName = do
  param1 <- lexeme Expression.stringExpression
  _ <- lexeme (Char.char ',')
  param2 <- lexeme Expression.stringExpression
  _ <- lexeme (Char.char ',')
  param3 <- Expression.stringExpression
  pure (Rumor.Action3 actionName param1 param2 param3)

action4 :: NonEmptyText -> Parser Rumor.Node
action4 actionName = do
  param1 <- lexeme Expression.stringExpression
  _ <- lexeme (Char.char ',')
  param2 <- lexeme Expression.stringExpression
  _ <- lexeme (Char.char ',')
  param3 <- lexeme Expression.stringExpression
  _ <- lexeme (Char.char ',')
  param4 <- Expression.stringExpression
  pure (Rumor.Action4 actionName param1 param2 param3 param4)
