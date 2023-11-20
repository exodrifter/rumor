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
-- >>> let parse inner = parseNodeTest newContext (inner <* eof)

{-| Parses an action with one to four string arguments.

  An action is a variable name followed by a set of parenthesis containing zero
  to four comma-separated arguments.

  >>> parse action "foobar()"
  let foobar: Action<>
  foobar()

  >>> parse action "foobar(\"1\")"
  let foobar: Action<String>
  foobar("1")

  >>> parse action "foobar(\"1\", \"2\")"
  let foobar: Action<String, String>
  foobar("1", "2")

  >>> parse action "foobar(\"1\", \"2\", \"3\")"
  let foobar: Action<String, String, String>
  foobar("1", "2", "3")

  >>> parse action "foobar(\"1\", \"2\", \"3\", \"4\")"
  let foobar: Action<String, String, String, String>
  foobar("1", "2", "3", "4")

  You can use any type you'd like as the arguments, as long as they don't
  conflict with pre-existing function definitions.

  >>> parse action "foobar(false || true, 1 + 2, \"Hello\")"
  let foobar: Action<Boolean, Number, String>
  foobar(false || true, 1.0 + 2.0, "Hello")

  No spaces are okay.

  >>> parse action "foobar(\"1\",\"2\")"
  let foobar: Action<String, String>
  foobar("1", "2")

  >>> parse action "foobar(\"1\",\"2\",\"3\")"
  let foobar: Action<String, String, String>
  foobar("1", "2", "3")

  >>> parse action "foobar(\"1\",\"2\",\"3\",\"4\")"
  let foobar: Action<String, String, String, String>
  foobar("1", "2", "3", "4")

  Extra spaces are okay.

  >>> parse action "foobar  (  )"
  let foobar: Action<>
  foobar()

  >>> parse action "foobar  (  \"1\"  )"
  let foobar: Action<String>
  foobar("1")

  >>> parse action "foobar  (  \"1\"  ,  \"2\"  )"
  let foobar: Action<String, String>
  foobar("1", "2")

  >>> parse action "foobar  (  \"1\"  ,  \"2\"  ,  \"3\"  )"
  let foobar: Action<String, String, String>
  foobar("1", "2", "3")

  >>> parse action "foobar  (  \"1\"  ,  \"2\"  ,  \"3\"  ,  \"4\"  )"
  let foobar: Action<String, String, String, String>
  foobar("1", "2", "3", "4")

  Extra newlines are okay.

  >>> parse action "foobar  (\n)"
  let foobar: Action<>
  foobar()

  >>> parse action "foobar  (\n\"1\"\n)"
  let foobar: Action<String>
  foobar("1")

  >>> parse action "foobar  (\n\"1\"\n,\n\"2\"\n)"
  let foobar: Action<String, String>
  foobar("1", "2")

  >>> parse action "foobar  (\n\"1\"\n,\n\"2\"\n,\n\"3\"\n)"
  let foobar: Action<String, String, String>
  foobar("1", "2", "3")

  >>> parse action "foobar  (\n\"1\"\n,\n\"2\"\n,\n\"3\"\n,\n\"4\"\n)"
  let foobar: Action<String, String, String, String>
  foobar("1", "2", "3", "4")

  Both parenthesis must be provided.

  >>> parse action "foobar("
  1:8:
    |
  1 | foobar(
    |        ^
  unexpected end of input
  expecting close parenthesis or expression

  >>> parse action "foobar)"
  1:7:
    |
  1 | foobar)
    |       ^
  unexpected ')'
  expecting open parenthesis or variable character

  Functions cannot start with a number.

  >>> parse action "123()"
  1:1:
    |
  1 | 123()
    | ^
  unexpected '1'
  expecting variable name

  Trailing whitespace is consumed.

  >>> parse action "foobar()  "
  let foobar: Action<>
  foobar()

  >>> parse action "foobar()  \n"
  let foobar: Action<>
  foobar()

  >>> parse action "foobar()  \n  "
  let foobar: Action<>
  foobar()
-}
action :: Parser Rumor.Node
action = do
  start <- Mega.getOffset
  actionName <- hlexeme Identifier.variableName
  end <- Mega.getOffset
  _ <- lexeme (Char.char '(') <?> "open parenthesis"

  result <-
        Mega.try (action4 start end actionName)
    <|> Mega.try (action3 start end actionName)
    <|> Mega.try (action2 start end actionName)
    <|> Mega.try (action1 start end actionName)
    <|> action0 start end actionName

  _ <- lexeme (Char.char ')') <?> "close parenthesis"
  pure result

action0 :: Int -> Int -> Rumor.VariableName -> Parser Rumor.Node
action0 start end actionName = do
  -- Typecheck the action
  TypeCheck.check
    (Rumor.ActionType [])
    (Rumor.AnnotatedVariable start end actionName)
  pure (Rumor.Action0 actionName)

action1 :: Int -> Int -> Rumor.VariableName -> Parser Rumor.Node
action1 start end actionName = do
  -- Parse the parameters
  param1 <- lexeme Expression.anyExpression <?> "expression"

  -- Typecheck the action
  type1 <- TypeCheck.infer param1
  TypeCheck.check
    (Rumor.ActionType [type1])
    (Rumor.AnnotatedVariable start end actionName)
  pure (Rumor.Action1 actionName (Rumor.unAnnotate param1))

action2 :: Int -> Int -> Rumor.VariableName -> Parser Rumor.Node
action2 start end actionName = do
  -- Parse the parameters
  param1 <- lexeme Expression.anyExpression <?> "expression"
  _ <- lexeme (Char.char ',')
  param2 <- lexeme Expression.anyExpression <?> "expression"

  -- Typecheck the action
  type1 <- TypeCheck.infer param1
  type2 <- TypeCheck.infer param2
  TypeCheck.check
    (Rumor.ActionType [type1, type2])
    (Rumor.AnnotatedVariable start end actionName)
  pure
    ( Rumor.Action2
        actionName
        (Rumor.unAnnotate param1)
        (Rumor.unAnnotate param2)
    )

action3 :: Int -> Int -> Rumor.VariableName -> Parser Rumor.Node
action3 start end actionName = do
  -- Parse the parameters
  param1 <- lexeme Expression.anyExpression <?> "expression"
  _ <- lexeme (Char.char ',')
  param2 <- lexeme Expression.anyExpression <?> "expression"
  _ <- lexeme (Char.char ',')
  param3 <- lexeme Expression.anyExpression <?> "expression"

  -- Typecheck the action
  type1 <- TypeCheck.infer param1
  type2 <- TypeCheck.infer param2
  type3 <- TypeCheck.infer param3
  TypeCheck.check
    (Rumor.ActionType [type1, type2, type3])
    (Rumor.AnnotatedVariable start end actionName)
  pure
    ( Rumor.Action3
        actionName
        (Rumor.unAnnotate param1)
        (Rumor.unAnnotate param2)
        (Rumor.unAnnotate param3)
    )

action4 :: Int -> Int -> Rumor.VariableName -> Parser Rumor.Node
action4 start end actionName = do
  -- Parse the parameters
  param1 <- lexeme Expression.anyExpression <?> "expression"
  _ <- lexeme (Char.char ',')
  param2 <- lexeme Expression.anyExpression <?> "expression"
  _ <- lexeme (Char.char ',')
  param3 <- lexeme Expression.anyExpression <?> "expression"
  _ <- lexeme (Char.char ',')
  param4 <- lexeme Expression.anyExpression <?> "expression"

  -- Typecheck the action
  type1 <- TypeCheck.infer param1
  type2 <- TypeCheck.infer param2
  type3 <- TypeCheck.infer param3
  type4 <- TypeCheck.infer param4
  TypeCheck.check
    (Rumor.ActionType [type1, type2, type3, type4])
    (Rumor.AnnotatedVariable start end actionName)
  pure
    ( Rumor.Action4
        actionName
        (Rumor.unAnnotate param1)
        (Rumor.unAnnotate param2)
        (Rumor.unAnnotate param3)
        (Rumor.unAnnotate param4)
    )
