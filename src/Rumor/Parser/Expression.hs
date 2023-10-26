{-# LANGUAGE GADTs #-}

module Rumor.Parser.Expression
( booleanExpression
, numberExpression
, stringExpression, stringEscapes, escape, interpolation
) where

import Data.Scientific (Scientific)
import Data.Text (Text)
import Rumor.Parser.Common (Parser, attempt, lexeme, modifyVariableType, (<?>), (<|>))

import qualified Control.Monad.State.Strict as State
import qualified Data.Text as T
import qualified Rumor.Internal as Rumor
import qualified Rumor.Parser.Identifier as Identifier
import qualified Rumor.Parser.Loose as Loose
import qualified Rumor.Parser.Surround as Surround
import qualified Text.Megaparsec as Mega
import qualified Text.Megaparsec.Char as Char

-- $setup
-- >>> import Data.Either (fromRight)
-- >>> import Data.NonEmptyText as NET
-- >>> import Rumor.Parser.Common
-- >>> import Rumor.Internal
--
-- >>> :{
-- let setVariableTypes c0 = do
--       c1 <- setVariableType (VariableName (Unicode (NET.new 's' "tring"))) StringType c0
--       c2 <- setVariableType (VariableName (Unicode (NET.new 'n' "umber"))) NumberType c1
--       c3 <- setVariableType (VariableName (Unicode (NET.new 'b' "oolean"))) BooleanType c2
--       pure c3
-- :}
--
-- >>> let context = fromRight undefined (setVariableTypes newContext)
-- >>> let parse inner = parseTest context (inner <* eof)

--------------------------------------------------------------------------------
-- Boolean
--------------------------------------------------------------------------------

{-| Parses a boolean expression. Any amount of space, including newlines, is
  allowed between the terms of the boolean expression.

  You can use variables, if they are of type Boolean.

  >>> parse booleanExpression "boolean and true"
  LogicalAnd (BooleanVariable (VariableName (Unicode "boolean"))) (Boolean True)

  >>> parse booleanExpression "number and true"
  1:1:
    |
  1 | number and true
    | ^^^^^^^^^^^^^^^
  Expected `number` to be of type Boolean, but it has been defined as or inferred to be a Number.

  >>> parse booleanExpression "string and true"
  1:1:
    |
  1 | string and true
    | ^^^^^^^^^^^^^^^
  Expected `string` to be of type Boolean, but it has been defined as or inferred to be a String.

  It can infer the types of undefined variables if the variables are used with a
  boolean operator or if the variable already has a known type.

  >>> parse booleanExpression "foo"
  1:1:
    |
  1 | foo
    | ^^^
  Cannot infer type of `foo`.

  >>> parse booleanExpression "boolean"
  BooleanVariable (VariableName (Unicode "boolean"))

  >>> parse booleanExpression "not foo"
  LogicalNot (BooleanVariable (VariableName (Unicode "foo")))

  It can only infer the types of equalities if at least one side has a known
  type.

  >>> parse booleanExpression "foo == 1"
  Equal (NumberVariable (VariableName (Unicode "foo"))) (Number 1.0)

  >>> parse booleanExpression "1 == foo"
  Equal (Number 1.0) (NumberVariable (VariableName (Unicode "foo")))

  >>> parse booleanExpression "foo == number"
  Equal (NumberVariable (VariableName (Unicode "foo"))) (NumberVariable (VariableName (Unicode "number")))

  >>> parse booleanExpression "number == foo"
  Equal (NumberVariable (VariableName (Unicode "number"))) (NumberVariable (VariableName (Unicode "foo")))

  >>> parse booleanExpression "foo == bar"
  1:1:
    |
  1 | foo == bar
    | ^^^^^^^^^^
  Cannot infer the type of this expression.

  >>> parse booleanExpression "number == boolean"
  1:1:
    |
  1 | number == boolean
    | ^^^^^^^^^^^^^^^^^
  Variable `boolean` cannot be a Number; it has already been defined as a Boolean!
-}
booleanExpression :: Parser (Rumor.Expression Bool)
booleanExpression =
  attempt do
    loose <- Loose.booleanLoose
    verifyTypes Rumor.toBoolean loose

{-| Parses a number expression. Any amount of space, including newlines, is
  allowed between the terms of the number expression.

  You can use variables, if they are of type Number.

  >>> parse numberExpression "number + 1"
  Addition (NumberVariable (VariableName (Unicode "number"))) (Number 1.0)

  >>> parse numberExpression "boolean + 1"
  1:1:
    |
  1 | boolean + 1
    | ^^^^^^^^^^^
  Expected `boolean` to be of type Number, but it has been defined as or inferred to be a Boolean.

  >>> parse numberExpression "string + 1"
  1:1:
    |
  1 | string + 1
    | ^^^^^^^^^^
  Expected `string` to be of type Number, but it has been defined as or inferred to be a String.

  It can infer the types of undefined variables if the variables are used with a
  boolean operator or if the variable already has a known type.

  >>> parse numberExpression "foo"
  1:1:
    |
  1 | foo
    | ^^^
  Cannot infer type of `foo`.

  >>> parse numberExpression "number"
  NumberVariable (VariableName (Unicode "number"))

  >>> parse numberExpression "foo + 1"
  Addition (NumberVariable (VariableName (Unicode "foo"))) (Number 1.0)
-}
numberExpression :: Parser (Rumor.Expression Scientific)
numberExpression =
  attempt do
    loose <- Loose.numberLoose
    verifyTypes Rumor.toNumber loose

{-| Converts a loosely-typed expression into a strictly-typed one as long as the
  variables in the strictly-typed expression do not conflict with existing
  variable definition.
-}
verifyTypes ::
  (Rumor.Context -> Rumor.Loose -> Either Rumor.InferenceFailure (Rumor.Expression a)) ->
  Rumor.Loose ->
  Parser (Either Text (Rumor.Expression a))
verifyTypes toExpression loose = do
  let
    go2 l r = do
      result <- go l
      case result of
        Left err -> pure (Left err)
        Right () -> go r

    go :: Rumor.Expression a -> Parser (Either Text ())
    go expr =
      case expr of
        Rumor.String _ -> pure (Right ())
        Rumor.StringVariable name -> modifyVariableType name Rumor.StringType
        Rumor.Concat l r -> go2 l r

        Rumor.Number _ -> pure (Right ())
        Rumor.NumberVariable name -> modifyVariableType name Rumor.NumberType
        Rumor.Addition l r -> go2 l r
        Rumor.Subtraction l r -> go2 l r
        Rumor.Multiplication l r -> go2 l r
        Rumor.Division l r -> go2 l r
        Rumor.NumberToString inner -> go inner

        Rumor.Boolean _ -> pure (Right ())
        Rumor.BooleanVariable name -> modifyVariableType name Rumor.BooleanType
        Rumor.LogicalNot inner -> go inner
        Rumor.LogicalAnd l r -> go2 l r
        Rumor.LogicalOr l r -> go2 l r
        Rumor.LogicalXor l r -> go2 l r
        Rumor.BooleanToString inner -> go inner

        Rumor.Equal l r -> go2 l r
        Rumor.NotEqual l r -> go2 l r

  context <- State.get
  case toExpression context loose of
    Left err ->
      pure (Left (Rumor.inferenceFailureToText err))
    Right expression -> do
      result <- go expression
      case result of
        Right () ->
          pure (Right expression)
        Left err ->
          pure (Left err)

--------------------------------------------------------------------------------
-- Text
--------------------------------------------------------------------------------

stringExpression :: Parser (Rumor.Expression Text)
stringExpression =
      string
  <|> variable Rumor.StringType Rumor.StringVariable

{-| Parses a string, which is any quoted string with interpolated values and no
  newlines.

  >>> parse stringExpression "\"Hello world!\""
  String "Hello world!"

  >>> parse stringExpression "\"I have { 5 } mangoes!\""
  Concat (String "I have ") (Concat (NumberToString (Number 5.0)) (String " mangoes!"))

  >>> parse stringExpression "\"Hello\\nworld!\""
  String "Hello\nworld!"

  You cannot use newlines in a string.

  >>> parse stringExpression "\"Hello\nworld!\"" -- Newlines are not okay
  1:7:
    |
  1 | "Hello
    |       ^
  unexpected newline
  expecting '\', close double quotes, interpolation, or literal char

  You must provide both the open and close double quotes.

  >>> parse stringExpression "\""
  1:2:
    |
  1 | "
    |  ^
  unexpected end of input
  expecting '\', close double quotes, interpolation, or literal char

  >>> parse stringExpression "\"Hello world!"
  1:14:
    |
  1 | "Hello world!
    |              ^
  unexpected end of input
  expecting '\', close double quotes, interpolation, or literal char

  >>> parse stringExpression "Hello world!\""
  1:6:
    |
  1 | Hello world!"
    |      ^
  unexpected space
  expecting end of input

  You must provide both the open and close brace for interpolated values
  >>> parse stringExpression "\"{\""
  1:4:
    |
  1 | "{"
    |    ^
  unexpected end of input
  expecting '\', close double quotes, interpolation, or literal char

  >>> parse stringExpression "\"}\""
  1:2:
    |
  1 | "}"
    |  ^
  unexpected '}'
  expecting '\', close double quotes, interpolation, or literal char

  This parser doesn't consume trailing whitespace.

  >>> parse stringExpression "\"Hello world!\"   "
  1:15:
    |
  1 | "Hello world!"
    |               ^
  unexpected space
  expecting end of input
-}
string :: Parser (Rumor.Expression Text)
string = do
  let
    literalString = do
      text <-
        Mega.takeWhile1P
          (Just "literal char")
          (`notElem` (fst <$> stringEscapes))
      pure (Rumor.String text)

  Surround.doubleQuotes do
    texts <- Mega.many
      (     Mega.try literalString
        <|> Mega.try (escape stringEscapes)
        <|> interpolation
      )
    pure (mconcat texts)

{-| The characters that can be escaped, and their corresponding escape code.
-}
stringEscapes :: [(Char, Text)]
stringEscapes =
  [ ('\n', "n")
  , ('\r', "r")
  , ('\\', "\\")
  , ('{', "{")
  , ('}', "}")
  , ('"', "\"")
  ]

{-| Parses a character escape sequence.

  >>> parse (escape stringEscapes) "\\n"
  String "\n"

  >>> parse (escape stringEscapes) "\\r"
  String "\r"

  >>> parse (escape stringEscapes) "\\\\"
  String "\\"

  >>> parse (escape stringEscapes) "\\{"
  String "{"

  >>> parse (escape stringEscapes) "\\}"
  String "}"

  >>> parse (escape stringEscapes) "\\\""
  String "\""

  >>> parse (escape stringEscapes) "\\"
  1:2:
    |
  1 | \
    |  ^
  unexpected end of input
  expecting '"', '\', 'n', 'r', '{', or '}'
-}
escape :: [(Char, Text)] -> Parser (Rumor.Expression Text)
escape escapes = do
  let
    escapeParser (ch, escapeCode) = do
      _ <- Char.string escapeCode
      pure (Rumor.String (T.singleton ch))

  _ <- Char.char '\\'
  Mega.choice (escapeParser <$> escapes)

{-| Parses a string interpolation, which is a boolean, number, or string
  expression surrounded by braces.

  Examples:
  >>> parse interpolation "{ true }"
  BooleanToString (Boolean True)

  >>> parse interpolation "{ true || false }"
  BooleanToString (LogicalOr (Boolean True) (Boolean False))

  >>> parse interpolation "{ 123 }"
  NumberToString (Number 123.0)

  >>> parse interpolation "{ 123 + 456 }"
  NumberToString (Addition (Number 123.0) (Number 456.0))

  >>> parse interpolation "{ \"foobar\" }"
  String "foobar"

  You can use whitespace between the braces and the expression.

  >>> parse interpolation "{true}" -- No whitespace is okay
  BooleanToString (Boolean True)

  >>> parse interpolation "{\ntrue\n}" -- Newlines are okay
  BooleanToString (Boolean True)

  The interpolation cannot be empty.

  >>> parse interpolation "{}"
  1:2:
    |
  1 | {}
    |  ^
  unexpected '}'
  expecting "false", "not", "true", '!', open double quotes, open parenthesis, signed number, or variable name

  You must provide both braces for an interpolation.

  >>> parse interpolation "{true"
  1:6:
    |
  1 | {true
    |      ^
  unexpected end of input
  expecting close brace

  >>> parse interpolation "true}"
  1:1:
    |
  1 | true}
    | ^
  unexpected 't'
  expecting interpolation
-}
interpolation :: Parser (Rumor.Expression Text)
interpolation =
  Surround.braces
    (     Mega.try (Rumor.BooleanToString <$> lexeme booleanExpression)
      <|> Mega.try (Rumor.NumberToString <$> lexeme numberExpression)
      <|> lexeme stringExpression
    ) <?> "interpolation"

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

variable ::
  Rumor.VariableType -> (Rumor.VariableName -> Rumor.Expression a) -> Parser (Rumor.Expression a)
variable typ constructor = do
  attempt do
    name <- Identifier.variableName
    result <- modifyVariableType name typ
    case result of
      Right () ->
        pure (Right (constructor name))
      Left err ->
        pure (Left err)
