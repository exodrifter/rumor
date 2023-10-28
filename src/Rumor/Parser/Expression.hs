module Rumor.Parser.Expression
( booleanLoose
, numberLoose
) where

import Data.Text (Text)
import Rumor.Parser.Common (Parser, hspace, lexeme, space, (<|>), (<?>))

import qualified Rumor.Internal as Rumor
import qualified Rumor.Parser.Surround as Surround
import qualified Rumor.Parser.Identifier as Identifier
import qualified Text.Megaparsec as Mega
import qualified Text.Megaparsec.Char as Char
import qualified Text.Megaparsec.Char.Lexer as Lexer
import qualified Text.Parser.Combinators as Combinators

-- $setup
-- >>> import Rumor.Parser.Common
-- >>> import Rumor.Internal
--
-- >>> let parse inner = parseTest newContext (inner <* eof)

{-| Parses a loosely-typed boolean expression. Any amount of space, including
  newlines, is allowed between the terms of the boolean expression.

  >>> parse booleanExpression "true"
  Boolean True

  >>> parse booleanExpression "false"
  Boolean False

  In order from highest to lowest precedence:

  >>> parse booleanExpression "not true"
  LogicalNot (Boolean True)

  >>> parse booleanExpression "true is false"
  Equal (Boolean True) (Boolean False)

  >>> parse booleanExpression "true == false"
  Equal (Boolean True) (Boolean False)

  >>> parse booleanExpression "true /= false"
  NotEqual (Boolean True) (Boolean False)

  >>> parse booleanExpression "true xor false"
  LogicalXor (Boolean True) (Boolean False)

  >>> parse booleanExpression "true ^ false"
  LogicalXor (Boolean True) (Boolean False)

  >>> parse booleanExpression "true and false"
  LogicalAnd (Boolean True) (Boolean False)

  >>> parse booleanExpression "true && false"
  LogicalAnd (Boolean True) (Boolean False)

  >>> parse booleanExpression "true or false"
  LogicalOr (Boolean True) (Boolean False)

  >>> parse booleanExpression "true || false"
  LogicalOr (Boolean True) (Boolean False)

  You can use parenthesis to change the precedence of the operations.

  >>> parse booleanExpression "true and true or false xor true"
  LogicalOr (LogicalAnd (Boolean True) (Boolean True)) (LogicalXor (Boolean False) (Boolean True))

  >>> parse booleanExpression "true and (true or false) xor true"
  LogicalAnd (Boolean True) (LogicalXor (LogicalOr (Boolean True) (Boolean False)) (Boolean True))

  You can also use variables.

  >>> parse booleanExpression "foobar"
  Variable (VariableName (Unicode "foobar"))

  >>> parse booleanExpression "foobar || true"
  LogicalOr (Variable (VariableName (Unicode "foobar"))) (Boolean True)

  >>> parse booleanExpression "foo == bar"
  Equal (Variable (VariableName (Unicode "foo"))) (Variable (VariableName (Unicode "bar")))

  >>> parse booleanExpression "foo == bar == baz"
  Equal (Equal (Variable (VariableName (Unicode "foo"))) (Variable (VariableName (Unicode "bar")))) (Variable (VariableName (Unicode "baz")))

  >>> parse booleanExpression "foo == bar == baz == biz"
  Equal (Equal (Variable (VariableName (Unicode "foo"))) (Variable (VariableName (Unicode "bar")))) (Equal (Variable (VariableName (Unicode "baz"))) (Variable (VariableName (Unicode "biz"))))

  You can use newlines.

  >>> parse booleanExpression "true\n||\nfalse"
  LogicalOr (Boolean True) (Boolean False)

  You don't need to use whitespace if you are using the non-word forms of the
  operators.

  >>> parse booleanExpression "true||false"
  LogicalOr (Boolean True) (Boolean False)

  >>> parse booleanExpression "trueorfalse"
  Variable (VariableName (Unicode "trueorfalse"))

  >>> parse booleanExpression "true&&false"
  LogicalAnd (Boolean True) (Boolean False)

  >>> parse booleanExpression "trueandfalse"
  Variable (VariableName (Unicode "trueandfalse"))

  >>> parse booleanExpression "true^false"
  LogicalXor (Boolean True) (Boolean False)

  >>> parse booleanExpression "truexorfalse"
  Variable (VariableName (Unicode "truexorfalse"))

  >>> parse booleanExpression "!false"
  LogicalNot (Boolean False)

  >>> parse booleanExpression "nottrue"
  Variable (VariableName (Unicode "nottrue"))

  >>> parse booleanExpression "true==true"
  Equal (Boolean True) (Boolean True)

  >>> parse booleanExpression "trueistrue"
  Variable (VariableName (Unicode "trueistrue"))

  The same applies for variables.

  >>> parse booleanExpression "foo||bar"
  LogicalOr (Variable (VariableName (Unicode "foo"))) (Variable (VariableName (Unicode "bar")))

  >>> parse booleanExpression "fooorbar"
  Variable (VariableName (Unicode "fooorbar"))

  >>> parse booleanExpression "foo&&bar"
  LogicalAnd (Variable (VariableName (Unicode "foo"))) (Variable (VariableName (Unicode "bar")))

  >>> parse booleanExpression "fooandbar"
  Variable (VariableName (Unicode "fooandbar"))

  >>> parse booleanExpression "foo^bar"
  LogicalXor (Variable (VariableName (Unicode "foo"))) (Variable (VariableName (Unicode "bar")))

  >>> parse booleanExpression "fooxorbar"
  Variable (VariableName (Unicode "fooxorbar"))

  >>> parse booleanExpression "!bar"
  LogicalNot (Variable (VariableName (Unicode "bar")))

  >>> parse booleanExpression "notfoo"
  Variable (VariableName (Unicode "notfoo"))

  >>> parse booleanExpression "foo==true"
  Equal (Variable (VariableName (Unicode "foo"))) (Boolean True)

  >>> parse booleanExpression "fooistrue"
  Variable (VariableName (Unicode "fooistrue"))

  You cannot write incomplete boolean expressions.

  >>> parse booleanExpression "&&"
  1:1:
    |
  1 | &&
    | ^^
  unexpected "&&"
  expecting "false", "not", "true", '!', open parenthesis, signed number, or variable name

  >>> parse booleanExpression "&& true"
  1:1:
    |
  1 | && true
    | ^^^^^
  unexpected "&& tr"
  expecting "false", "not", "true", '!', open parenthesis, signed number, or variable name

  >>> parse booleanExpression "true &&"
  1:8:
    |
  1 | true &&
    |        ^
  unexpected end of input
  expecting "false", "not", "true", '!', open parenthesis, signed number, or variable name

  This parser doesn't consume trailing whitespace.

  >>> parse booleanExpression "true  \n  "
  1:5:
    |
  1 | true
    |     ^
  unexpected space
  expecting end of input
-}
booleanExpression :: Parser Rumor.Expression
booleanExpression =
  let
    -- Parse a boolean expression with the left associative operators from
    -- highest to lowest precedence.
    expression = factor
      `Combinators.chainl1` discardWhitespace eqOperator
      `Combinators.chainl1` discardWhitespace neqOperator
      `Combinators.chainl1` discardWhitespace xorOperator
      `Combinators.chainl1` discardWhitespace andOperator
      `Combinators.chainl1` discardWhitespace orOperator

    factor =
          Mega.try (Surround.parentheses expression)
      <|> Mega.try (notOperator term)
      <|> Mega.try equalityExpression
      <|> term

    term =
          Mega.try boolean
      <|> variable

    notOperator inner = do
      _ <- discardWhitespace ("!" <|> do _ <- "not"; " ")
      Rumor.LogicalNot <$> inner
    eqOperator = do
      _ <- "==" <|> "is"
      pure Rumor.Equal
    neqOperator = do
      _ <- "/="
      pure Rumor.NotEqual
    xorOperator = do
      _ <- "^" <|> "xor"
      pure Rumor.LogicalXor
    andOperator = do
      _ <- "&&" <|> "and"
      pure Rumor.LogicalAnd
    orOperator = do
      _ <- "||" <|> "or"
      pure Rumor.LogicalOr

  in
    expression

boolean :: Parser Rumor.Expression
boolean =
  let
    true = do
      _ <- "true"
      pure (Rumor.Boolean True)
    false = do
      _ <- "false"
      pure (Rumor.Boolean False)

    -- Slightly nicer error messages compared to `Mega.notFollowedBy`.
    notFollowedBy :: Parser Text -> Parser ()
    notFollowedBy str =
      Mega.notFollowedBy str <|> (do _ <- " "; pure ())

  in do
    result <- true <|> false

    -- literals cannot be followed by any of the following
    notFollowedBy "or"
    notFollowedBy "and"
    notFollowedBy "xor"
    notFollowedBy "is"

    pure result

{-| Parses a loosely-typed equality expression. Any amount of space, including
  newlines, is allowed between the terms of the equality expression.

  >>> parse equalityExpression "foo == bar"
  Equal (Variable (VariableName (Unicode "foo"))) (Variable (VariableName (Unicode "bar")))

  >>> parse equalityExpression "foo /= bar"
  NotEqual (Variable (VariableName (Unicode "foo"))) (Variable (VariableName (Unicode "bar")))
-}
equalityExpression :: Parser Rumor.Expression
equalityExpression = do
  let
    term =
          Mega.try (Surround.parentheses equalityExpression)
      <|> Mega.try number
      <|> Rumor.Variable <$> Identifier.variableName

    eqOperator = do
      _ <- "==" <|> "is"
      pure Rumor.Equal
    neqOperator = do
      _ <- "/="
      pure Rumor.NotEqual

  l <- lexeme term
  op <- lexeme (eqOperator <|> neqOperator)
  r <- lexeme term

  pure (op l r)

--------------------------------------------------------------------------------
-- Number
--------------------------------------------------------------------------------

{-| Parses a mathematical expression. Any amount of space, including newlines,
  is allowed between the terms of the mathematical expression.

  >>> parse numberExpression "1"
  Number 1.0

  >>> parse numberExpression "-1"
  Number (-1.0)

  In order from highest to lowest precedence:

  >>> parse numberExpression "1 * 2"
  Multiplication (Number 1.0) (Number 2.0)

  >>> parse numberExpression "1 / 2"
  Division (Number 1.0) (Number 2.0)

  >>> parse numberExpression "1 + 2"
  Addition (Number 1.0) (Number 2.0)

  >>> parse numberExpression "1 - 2"
  Subtraction (Number 1.0) (Number 2.0)

  You can use parenthesis to change the precedence of the operations.

  >>> parse numberExpression "1 * 2 + 3 / -4"
  Addition (Multiplication (Number 1.0) (Number 2.0)) (Division (Number 3.0) (Number (-4.0)))

  >>> parse numberExpression "1 * (2 + 3) / -4"
  Division (Multiplication (Number 1.0) (Addition (Number 2.0) (Number 3.0))) (Number (-4.0))

  You can use negative signs in an expression.

  >>> parse numberExpression "-1 - -2"
  Subtraction (Number (-1.0)) (Number (-2.0))

  You can also use variables.

  >>> parse numberExpression "foobar"
  Variable (VariableName (Unicode "foobar"))

  >>> parse numberExpression "foobar + 1.0"
  Addition (Variable (VariableName (Unicode "foobar"))) (Number 1.0)

  You can use no whitespace or additional newlines:

  >>> parse numberExpression "1*3--4/2" -- No whitespace is okay
  Subtraction (Multiplication (Number 1.0) (Number 3.0)) (Division (Number (-4.0)) (Number 2.0))

  >>> parse numberExpression "1\n*\n3\n-\n-4\n/\n2" -- Newlines are okay
  Subtraction (Multiplication (Number 1.0) (Number 3.0)) (Division (Number (-4.0)) (Number 2.0))

  You cannot write incomplete number expressions.

  >>> parse numberExpression "*"
  1:1:
    |
  1 | *
    | ^
  unexpected '*'
  expecting open parenthesis, signed number, or variable name

  >>> parse numberExpression "1 * 2 + 3 /"
  1:12:
    |
  1 | 1 * 2 + 3 /
    |            ^
  unexpected end of input
  expecting open parenthesis, signed number, or variable name

  You cannot use the `/=` operator.

  >>> parse numberExpression "1 * 2 + 3 /= 4"
  1:12:
    |
  1 | 1 * 2 + 3 /= 4
    |            ^
  unexpected '='

  This parser doesn't consume trailing whitespace.

  >>> parse numberExpression "1 * 2  \n  "
  1:6:
    |
  1 | 1 * 2
    |      ^
  unexpected space
  expecting '.', 'E', 'e', digit, or end of input

-}
numberExpression :: Parser Rumor.Expression
numberExpression =
  let
    expression = term
      `Combinators.chainl1` discardWhitespace multiplicationOperator
      `Combinators.chainl1` divisionOperator
      `Combinators.chainl1` discardWhitespace additionOperator
      `Combinators.chainl1` discardWhitespace subtractionOperator
    term =
          Surround.parentheses expression
      <|> number
      <|> variable

    multiplicationOperator = do
      _ <- Char.char '*'
      pure Rumor.Multiplication
    divisionOperator = do
      Mega.try do
        space
        _ <- Char.char '/'
        pure ()

      -- Ensure this isn't a `/=` operator
      Mega.notFollowedBy (Char.char '=')
      space
      pure Rumor.Division
    additionOperator = do
      _ <- Char.char '+'
      pure Rumor.Addition
    subtractionOperator = do
      _ <- Char.char '-'
      pure Rumor.Subtraction

  in
    expression

{-| Parses a number literal, which can be any signed decimal number. It can be
  written using scientific notation.

  >>> parse number "1"
  Number 1.0

  >>> parse number "1.0"
  Number 1.0

  >>> parse number "+1.0"
  Number 1.0

  >>> parse number "-1.0"
  Number (-1.0)

  >>> parse number "+1"
  Number 1.0

  >>> parse number "-1"
  Number (-1.0)

  >>> parse number "1e10"
  Number 1.0e10

  >>> parse number "+1e10"
  Number 1.0e10

  >>> parse number "-1e10"
  Number (-1.0e10)

  Error examples:
  >>> parse number "1."
  1:2:
    |
  1 | 1.
    |  ^
  unexpected '.'
  expecting 'E', 'e', digit, or end of input

  >>> parse number ".1"
  1:1:
    |
  1 | .1
    | ^
  unexpected '.'
  expecting signed number
-}
number :: Parser Rumor.Expression
number = do
  n <- Lexer.signed hspace Lexer.scientific <?> "signed number"
  pure (Rumor.Number n)

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

variable :: Parser Rumor.Expression
variable = Rumor.Variable <$> Identifier.variableName

-- Discards whitespace surrounding an operator on both sides
discardWhitespace :: Parser a -> Parser a
discardWhitespace operator = do
  Mega.try do
    space
    lexeme operator
