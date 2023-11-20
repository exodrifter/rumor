module Rumor.Parser.Action
( action
) where

import Rumor.Parser.Common (Parser, hlexeme, lexeme, (<?>))

import qualified Rumor.Internal as Rumor
import qualified Rumor.Parser.Expression as Expression
import qualified Rumor.Parser.Identifier as Identifier
import qualified Rumor.TypeCheck as TypeCheck
import qualified Text.Megaparsec as Mega
import qualified Text.Megaparsec.Char as Char

-- $setup
-- >>> import Rumor.Parser.Common
-- >>> let parse inner = parseNodeTest newContext (inner <* eof)

{-| Parses an action with any number of arguments.

  An action is a variable name followed by a set of parenthesis containing any
  number of comma-separated arguments.

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

  Trailing horizontal whitespace is consumed.

  >>> parse action "foobar()  "
  let foobar: Action<>
  foobar()

  >>> parse action "foobar()  \n"
  1:11:
    |
  1 | foobar()
    |           ^
  unexpected newline
  expecting end of input
-}
action :: Parser Rumor.Node
action = do
  -- Parse the action
  start <- Mega.getOffset
  actionName <- hlexeme Identifier.variableName
  end <- Mega.getOffset
  _ <- lexeme (Char.char '(') <?> "open parenthesis"
  params <- parameters
  _ <- hlexeme (Char.char ')') <?> "close parenthesis"

  -- Typecheck the action
  types <- traverse TypeCheck.infer params
  TypeCheck.check
    (Rumor.ActionType types)
    (Rumor.AnnotatedVariable start end actionName)
  pure (Rumor.Action actionName (Rumor.unAnnotate <$> params))

parameters :: Parser [Rumor.AnnotatedExpression]
parameters = do
  -- Check if there is a parameter
  mParam <- Mega.optional (lexeme Expression.anyExpression) <?> "expression"
  case mParam of
    Nothing ->
      pure []

    -- If there is a parameter, check if it is followed by another parameter.
    Just param -> do
      comma <- Mega.optional (lexeme (Char.char ','))
      case comma of
        Just _ -> do
          rest <- parameters
          pure (param : rest)
        Nothing ->
          pure [param]
