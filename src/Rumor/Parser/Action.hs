module Rumor.Parser.Action
( action
) where

import Rumor.Parser.Common (Parser, hlexeme, lexeme, (<?>), (<|>))

import qualified Rumor.Internal as Rumor
import qualified Rumor.Parser.Expression as Expression
import qualified Rumor.Parser.Identifier as Identifier
import qualified Rumor.TypeCheck as TypeCheck
import qualified Text.Megaparsec as Mega
import qualified Text.Megaparsec.Char as Char

-- $setup
-- >>> import Rumor.Parser.Common
-- >>> let parse inner = parseTest newContext (inner <* eof)

{-| Parses an action with one to four string arguments.

  TODO: Support other argument types

  An action is a variable name followed by a set of parenthesis containing zero
  to four comma-separated arguments.

  >>> parse action "foobar()"
  Action0 (VariableName (Unicode "foobar"))

  >>> parse action "123()"
  1:1:
    |
  1 | 123()
    | ^
  unexpected '1'
  expecting variable name

  >>> parse action "foobar(\"1\")"
  Action1 (VariableName (Unicode "foobar")) (String "1")

  >>> parse action "foobar(\"1\", \"2\")"
  Action2 (VariableName (Unicode "foobar")) (String "1") (String "2")

  >>> parse action "foobar(\"1\", \"2\", \"3\")"
  Action3 (VariableName (Unicode "foobar")) (String "1") (String "2") (String "3")

  >>> parse action "foobar(\"1\", \"2\", \"3\", \"4\")"
  Action4 (VariableName (Unicode "foobar")) (String "1") (String "2") (String "3") (String "4")

  No spaces are okay.

  >>> parse action "foobar(\"1\",\"2\")"
  Action2 (VariableName (Unicode "foobar")) (String "1") (String "2")

  >>> parse action "foobar(\"1\",\"2\",\"3\")"
  Action3 (VariableName (Unicode "foobar")) (String "1") (String "2") (String "3")

  >>> parse action "foobar(\"1\",\"2\",\"3\",\"4\")"
  Action4 (VariableName (Unicode "foobar")) (String "1") (String "2") (String "3") (String "4")

  Extra spaces are okay.

  >>> parse action "foobar  (  )"
  Action0 (VariableName (Unicode "foobar"))

  >>> parse action "foobar  (  \"1\"  )"
  Action1 (VariableName (Unicode "foobar")) (String "1")

  >>> parse action "foobar  (  \"1\"  ,  \"2\"  )"
  Action2 (VariableName (Unicode "foobar")) (String "1") (String "2")

  >>> parse action "foobar  (  \"1\"  ,  \"2\"  ,  \"3\"  )"
  Action3 (VariableName (Unicode "foobar")) (String "1") (String "2") (String "3")

  >>> parse action "foobar  (  \"1\"  ,  \"2\"  ,  \"3\"  ,  \"4\"  )"
  Action4 (VariableName (Unicode "foobar")) (String "1") (String "2") (String "3") (String "4")

  Extra newlines are okay.

  >>> parse action "foobar  (\n)"
  Action0 (VariableName (Unicode "foobar"))

  >>> parse action "foobar  (\n\"1\"\n)"
  Action1 (VariableName (Unicode "foobar")) (String "1")

  >>> parse action "foobar  (\n\"1\"\n,\n\"2\"\n)"
  Action2 (VariableName (Unicode "foobar")) (String "1") (String "2")

  >>> parse action "foobar  (\n\"1\"\n,\n\"2\"\n,\n\"3\"\n)"
  Action3 (VariableName (Unicode "foobar")) (String "1") (String "2") (String "3")

  >>> parse action "foobar  (\n\"1\"\n,\n\"2\"\n,\n\"3\"\n,\n\"4\"\n)"
  Action4 (VariableName (Unicode "foobar")) (String "1") (String "2") (String "3") (String "4")

  Both parenthesis must be provided.

  >>> parse action "foobar("
  1:8:
    |
  1 | foobar(
    |        ^
  unexpected end of input
  expecting "false", "not", "true", '!', close parenthesis, open double quotes, open parenthesis, signed number, or variable name

  >>> parse action "foobar)"
  1:7:
    |
  1 | foobar)
    |       ^
  unexpected ')'
  expecting open parenthesis or variable character

  Trailing whitespace is consumed.

  >>> parse action "foobar()  "
  Action0 (VariableName (Unicode "foobar"))

  >>> parse action "foobar()  \n"
  Action0 (VariableName (Unicode "foobar"))

  >>> parse action "foobar()  \n  "
  Action0 (VariableName (Unicode "foobar"))
-}
action :: Parser Rumor.Node
action = do
  actionName <- hlexeme Identifier.variableName
  _ <- lexeme (Char.char '(') <?> "open parenthesis"

  result <-
        Mega.try (lexeme (action4 actionName))
    <|> Mega.try (lexeme (action3 actionName))
    <|> Mega.try (lexeme (action2 actionName))
    <|> Mega.try (lexeme (action1 actionName))
    <|> pure (Rumor.Action0 actionName)

  _ <- lexeme (Char.char ')') <?> "close parenthesis"
  pure result

action1 :: Rumor.VariableName -> Parser Rumor.Node
action1 actionName = do
  param1 <- Expression.anyExpression
  _ <- TypeCheck.infer param1
  -- TODO: typecheck the function call
  pure (Rumor.Action1 actionName (Rumor.unAnnotate param1))

action2 :: Rumor.VariableName -> Parser Rumor.Node
action2 actionName = do
  param1 <- lexeme Expression.anyExpression
  _ <- TypeCheck.infer param1
  _ <- lexeme (Char.char ',')
  param2 <- Expression.anyExpression
  _ <- TypeCheck.infer param2
  -- TODO: typecheck the function call
  pure (Rumor.Action2 actionName (Rumor.unAnnotate param1) (Rumor.unAnnotate param2))

action3 :: Rumor.VariableName -> Parser Rumor.Node
action3 actionName = do
  param1 <- lexeme Expression.anyExpression
  _ <- TypeCheck.infer param1
  _ <- lexeme (Char.char ',')
  param2 <- lexeme Expression.anyExpression
  _ <- TypeCheck.infer param2
  _ <- lexeme (Char.char ',')
  param3 <- Expression.anyExpression
  _ <- TypeCheck.infer param3
  -- TODO: typecheck the function call
  pure (Rumor.Action3 actionName (Rumor.unAnnotate param1) (Rumor.unAnnotate param2) (Rumor.unAnnotate param3))

action4 :: Rumor.VariableName -> Parser Rumor.Node
action4 actionName = do
  param1 <- lexeme Expression.anyExpression
  _ <- TypeCheck.infer param1
  _ <- lexeme (Char.char ',')
  param2 <- lexeme Expression.anyExpression
  _ <- TypeCheck.infer param2
  _ <- lexeme (Char.char ',')
  param3 <- lexeme Expression.anyExpression
  _ <- TypeCheck.infer param3
  _ <- lexeme (Char.char ',')
  param4 <- Expression.anyExpression
  _ <- TypeCheck.infer param4
  -- TODO: typecheck the function call
  pure (Rumor.Action4 actionName (Rumor.unAnnotate param1) (Rumor.unAnnotate param2) (Rumor.unAnnotate param3) (Rumor.unAnnotate param4))
