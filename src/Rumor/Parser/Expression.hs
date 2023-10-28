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

  >>> parse booleanLoose "true"
  BooleanLiteral True

  >>> parse booleanLoose "false"
  BooleanLiteral False

  In order from highest to lowest precedence:

  >>> parse booleanLoose "not true"
  LooseLogicalNot (BooleanLiteral True)

  >>> parse booleanLoose "true is false"
  LooseEqual (BooleanLiteral True) (BooleanLiteral False)

  >>> parse booleanLoose "true == false"
  LooseEqual (BooleanLiteral True) (BooleanLiteral False)

  >>> parse booleanLoose "true /= false"
  LooseNotEqual (BooleanLiteral True) (BooleanLiteral False)

  >>> parse booleanLoose "true xor false"
  LooseLogicalXor (BooleanLiteral True) (BooleanLiteral False)

  >>> parse booleanLoose "true ^ false"
  LooseLogicalXor (BooleanLiteral True) (BooleanLiteral False)

  >>> parse booleanLoose "true and false"
  LooseLogicalAnd (BooleanLiteral True) (BooleanLiteral False)

  >>> parse booleanLoose "true && false"
  LooseLogicalAnd (BooleanLiteral True) (BooleanLiteral False)

  >>> parse booleanLoose "true or false"
  LooseLogicalOr (BooleanLiteral True) (BooleanLiteral False)

  >>> parse booleanLoose "true || false"
  LooseLogicalOr (BooleanLiteral True) (BooleanLiteral False)

  You can use parenthesis to change the precedence of the operations.

  >>> parse booleanLoose "true and true or false xor true"
  LooseLogicalOr (LooseLogicalAnd (BooleanLiteral True) (BooleanLiteral True)) (LooseLogicalXor (BooleanLiteral False) (BooleanLiteral True))

  >>> parse booleanLoose "true and (true or false) xor true"
  LooseLogicalAnd (BooleanLiteral True) (LooseLogicalXor (LooseLogicalOr (BooleanLiteral True) (BooleanLiteral False)) (BooleanLiteral True))

  You can also use variables.

  >>> parse booleanLoose "foobar"
  LooseVariable (VariableName (Unicode "foobar"))

  >>> parse booleanLoose "foobar || true"
  LooseLogicalOr (LooseVariable (VariableName (Unicode "foobar"))) (BooleanLiteral True)

  >>> parse booleanLoose "foo == bar"
  LooseEqual (LooseVariable (VariableName (Unicode "foo"))) (LooseVariable (VariableName (Unicode "bar")))

  >>> parse booleanLoose "foo == bar == baz"
  LooseEqual (LooseEqual (LooseVariable (VariableName (Unicode "foo"))) (LooseVariable (VariableName (Unicode "bar")))) (LooseVariable (VariableName (Unicode "baz")))

  >>> parse booleanLoose "foo == bar == baz == biz"
  LooseEqual (LooseEqual (LooseVariable (VariableName (Unicode "foo"))) (LooseVariable (VariableName (Unicode "bar")))) (LooseEqual (LooseVariable (VariableName (Unicode "baz"))) (LooseVariable (VariableName (Unicode "biz"))))

  You can use newlines.

  >>> parse booleanLoose "true\n||\nfalse"
  LooseLogicalOr (BooleanLiteral True) (BooleanLiteral False)

  You don't need to use whitespace if you are using the non-word forms of the
  operators.

  >>> parse booleanLoose "true||false"
  LooseLogicalOr (BooleanLiteral True) (BooleanLiteral False)

  >>> parse booleanLoose "trueorfalse"
  LooseVariable (VariableName (Unicode "trueorfalse"))

  >>> parse booleanLoose "true&&false"
  LooseLogicalAnd (BooleanLiteral True) (BooleanLiteral False)

  >>> parse booleanLoose "trueandfalse"
  LooseVariable (VariableName (Unicode "trueandfalse"))

  >>> parse booleanLoose "true^false"
  LooseLogicalXor (BooleanLiteral True) (BooleanLiteral False)

  >>> parse booleanLoose "truexorfalse"
  LooseVariable (VariableName (Unicode "truexorfalse"))

  >>> parse booleanLoose "!false"
  LooseLogicalNot (BooleanLiteral False)

  >>> parse booleanLoose "nottrue"
  LooseVariable (VariableName (Unicode "nottrue"))

  >>> parse booleanLoose "true==true"
  LooseEqual (BooleanLiteral True) (BooleanLiteral True)

  >>> parse booleanLoose "trueistrue"
  LooseVariable (VariableName (Unicode "trueistrue"))

  The same applies for variables.

  >>> parse booleanLoose "foo||bar"
  LooseLogicalOr (LooseVariable (VariableName (Unicode "foo"))) (LooseVariable (VariableName (Unicode "bar")))

  >>> parse booleanLoose "fooorbar"
  LooseVariable (VariableName (Unicode "fooorbar"))

  >>> parse booleanLoose "foo&&bar"
  LooseLogicalAnd (LooseVariable (VariableName (Unicode "foo"))) (LooseVariable (VariableName (Unicode "bar")))

  >>> parse booleanLoose "fooandbar"
  LooseVariable (VariableName (Unicode "fooandbar"))

  >>> parse booleanLoose "foo^bar"
  LooseLogicalXor (LooseVariable (VariableName (Unicode "foo"))) (LooseVariable (VariableName (Unicode "bar")))

  >>> parse booleanLoose "fooxorbar"
  LooseVariable (VariableName (Unicode "fooxorbar"))

  >>> parse booleanLoose "!bar"
  LooseLogicalNot (LooseVariable (VariableName (Unicode "bar")))

  >>> parse booleanLoose "notfoo"
  LooseVariable (VariableName (Unicode "notfoo"))

  >>> parse booleanLoose "foo==true"
  LooseEqual (LooseVariable (VariableName (Unicode "foo"))) (BooleanLiteral True)

  >>> parse booleanLoose "fooistrue"
  LooseVariable (VariableName (Unicode "fooistrue"))

  You cannot write incomplete boolean expressions.

  >>> parse booleanLoose "&&"
  1:1:
    |
  1 | &&
    | ^^
  unexpected "&&"
  expecting "false", "not", "true", '!', open parenthesis, signed number, or variable name

  >>> parse booleanLoose "&& true"
  1:1:
    |
  1 | && true
    | ^^^^^
  unexpected "&& tr"
  expecting "false", "not", "true", '!', open parenthesis, signed number, or variable name

  >>> parse booleanLoose "true &&"
  1:8:
    |
  1 | true &&
    |        ^
  unexpected end of input
  expecting "false", "not", "true", '!', open parenthesis, signed number, or variable name

  This parser doesn't consume trailing whitespace.

  >>> parse booleanLoose "true  \n  "
  1:5:
    |
  1 | true
    |     ^
  unexpected space
  expecting end of input
-}
booleanLoose :: Parser Rumor.Loose
booleanLoose =
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
      <|> Mega.try equalityLoose
      <|> term

    term =
          Mega.try boolean
      <|> variable

    notOperator inner = do
      _ <- discardWhitespace ("!" <|> do _ <- "not"; " ")
      Rumor.LooseLogicalNot <$> inner
    eqOperator = do
      _ <- "==" <|> "is"
      pure Rumor.LooseEqual
    neqOperator = do
      _ <- "/="
      pure Rumor.LooseNotEqual
    xorOperator = do
      _ <- "^" <|> "xor"
      pure Rumor.LooseLogicalXor
    andOperator = do
      _ <- "&&" <|> "and"
      pure Rumor.LooseLogicalAnd
    orOperator = do
      _ <- "||" <|> "or"
      pure Rumor.LooseLogicalOr

  in
    expression

boolean :: Parser Rumor.Loose
boolean =
  let
    true = do
      _ <- "true"
      pure (Rumor.BooleanLiteral True)
    false = do
      _ <- "false"
      pure (Rumor.BooleanLiteral False)

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

--------------------------------------------------------------------------------
-- Equality
--------------------------------------------------------------------------------

{-| Parses a loosely-typed equality expression. Any amount of space, including
  newlines, is allowed between the terms of the equality expression.

  >>> parse equalityLoose "foo == bar"
  LooseEqual (LooseVariable (VariableName (Unicode "foo"))) (LooseVariable (VariableName (Unicode "bar")))

  >>> parse equalityLoose "foo /= bar"
  LooseNotEqual (LooseVariable (VariableName (Unicode "foo"))) (LooseVariable (VariableName (Unicode "bar")))
-}
equalityLoose :: Parser Rumor.Loose
equalityLoose = do
  let
    term =
          Mega.try (Surround.parentheses equalityLoose)
      <|> Mega.try number
      <|> Rumor.LooseVariable <$> Identifier.variableName

    eqOperator = do
      _ <- "==" <|> "is"
      pure Rumor.LooseEqual
    neqOperator = do
      _ <- "/="
      pure Rumor.LooseNotEqual

  l <- lexeme term
  op <- lexeme (eqOperator <|> neqOperator)
  r <- lexeme term

  pure (op l r)

--------------------------------------------------------------------------------
-- Number
--------------------------------------------------------------------------------

{-| Parses a mathematical expression. Any amount of space, including newlines,
  is allowed between the terms of the mathematical expression.

  >>> parse numberLoose "1"
  NumberLiteral 1.0

  >>> parse numberLoose "-1"
  NumberLiteral (-1.0)

  In order from highest to lowest precedence:

  >>> parse numberLoose "1 * 2"
  LooseMultiplication (NumberLiteral 1.0) (NumberLiteral 2.0)

  >>> parse numberLoose "1 / 2"
  LooseDivision (NumberLiteral 1.0) (NumberLiteral 2.0)

  >>> parse numberLoose "1 + 2"
  LooseAddition (NumberLiteral 1.0) (NumberLiteral 2.0)

  >>> parse numberLoose "1 - 2"
  LooseSubtraction (NumberLiteral 1.0) (NumberLiteral 2.0)

  You can use parenthesis to change the precedence of the operations.

  >>> parse numberLoose "1 * 2 + 3 / -4"
  LooseAddition (LooseMultiplication (NumberLiteral 1.0) (NumberLiteral 2.0)) (LooseDivision (NumberLiteral 3.0) (NumberLiteral (-4.0)))

  >>> parse numberLoose "1 * (2 + 3) / -4"
  LooseDivision (LooseMultiplication (NumberLiteral 1.0) (LooseAddition (NumberLiteral 2.0) (NumberLiteral 3.0))) (NumberLiteral (-4.0))

  You can use negative signs in an expression.

  >>> parse numberLoose "-1 - -2"
  LooseSubtraction (NumberLiteral (-1.0)) (NumberLiteral (-2.0))

  You can also use variables.

  >>> parse numberLoose "foobar"
  LooseVariable (VariableName (Unicode "foobar"))

  >>> parse numberLoose "foobar + 1.0"
  LooseAddition (LooseVariable (VariableName (Unicode "foobar"))) (NumberLiteral 1.0)

  You can use no whitespace or additional newlines:

  >>> parse numberLoose "1*3--4/2" -- No whitespace is okay
  LooseSubtraction (LooseMultiplication (NumberLiteral 1.0) (NumberLiteral 3.0)) (LooseDivision (NumberLiteral (-4.0)) (NumberLiteral 2.0))

  >>> parse numberLoose "1\n*\n3\n-\n-4\n/\n2" -- Newlines are okay
  LooseSubtraction (LooseMultiplication (NumberLiteral 1.0) (NumberLiteral 3.0)) (LooseDivision (NumberLiteral (-4.0)) (NumberLiteral 2.0))

  You cannot write incomplete number expressions.

  >>> parse numberLoose "*"
  1:1:
    |
  1 | *
    | ^
  unexpected '*'
  expecting open parenthesis, signed number, or variable name

  >>> parse numberLoose "1 * 2 + 3 /"
  1:12:
    |
  1 | 1 * 2 + 3 /
    |            ^
  unexpected end of input
  expecting open parenthesis, signed number, or variable name

  You cannot use the `/=` operator.

  >>> parse numberLoose "1 * 2 + 3 /= 4"
  1:12:
    |
  1 | 1 * 2 + 3 /= 4
    |            ^
  unexpected '='

  This parser doesn't consume trailing whitespace.

  >>> parse numberLoose "1 * 2  \n  "
  1:6:
    |
  1 | 1 * 2
    |      ^
  unexpected space
  expecting '.', 'E', 'e', digit, or end of input

-}
numberLoose :: Parser Rumor.Loose
numberLoose =
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
      pure Rumor.LooseMultiplication
    divisionOperator = do
      Mega.try do
        space
        _ <- Char.char '/'
        pure ()

      -- Ensure this isn't a `/=` operator
      Mega.notFollowedBy (Char.char '=')
      space
      pure Rumor.LooseDivision
    additionOperator = do
      _ <- Char.char '+'
      pure Rumor.LooseAddition
    subtractionOperator = do
      _ <- Char.char '-'
      pure Rumor.LooseSubtraction

  in
    expression

{-| Parses a number literal, which can be any signed decimal number. It can be
  written using scientific notation.

  >>> parse number "1"
  NumberLiteral 1.0

  >>> parse number "1.0"
  NumberLiteral 1.0

  >>> parse number "+1.0"
  NumberLiteral 1.0

  >>> parse number "-1.0"
  NumberLiteral (-1.0)

  >>> parse number "+1"
  NumberLiteral 1.0

  >>> parse number "-1"
  NumberLiteral (-1.0)

  >>> parse number "1e10"
  NumberLiteral 1.0e10

  >>> parse number "+1e10"
  NumberLiteral 1.0e10

  >>> parse number "-1e10"
  NumberLiteral (-1.0e10)

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
number :: Parser Rumor.Loose
number = do
  n <- Lexer.signed hspace Lexer.scientific <?> "signed number"
  pure (Rumor.NumberLiteral n)

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

variable :: Parser Rumor.Loose
variable = Rumor.LooseVariable <$> Identifier.variableName

-- Discards whitespace surrounding an operator on both sides
discardWhitespace :: Parser a -> Parser a
discardWhitespace operator = do
  Mega.try do
    space
    lexeme operator
