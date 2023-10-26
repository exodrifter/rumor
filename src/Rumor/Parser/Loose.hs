module Rumor.Parser.Loose
( booleanLoose
, equalityLoose
) where

import Data.Text (Text)
import Rumor.Parser.Common (Parser, lexeme, space, (<|>))

import qualified Rumor.Internal as Rumor
import qualified Rumor.Parser.Surround as Surround
import qualified Rumor.Parser.Identifier as Identifier
import qualified Text.Megaparsec as Mega
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
  expecting "false", "not", "true", '!', open parenthesis, or variable name

  >>> parse booleanLoose "&& true"
  1:1:
    |
  1 | && true
    | ^^^^^
  unexpected "&& tr"
  expecting "false", "not", "true", '!', open parenthesis, or variable name

  >>> parse booleanLoose "true &&"
  1:8:
    |
  1 | true &&
    |        ^
  unexpected end of input
  expecting "false", "not", "true", '!', open parenthesis, or variable name

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
    expression = term
      `Combinators.chainl1` discardWhitespace eqOperator
      `Combinators.chainl1` discardWhitespace neqOperator
      `Combinators.chainl1` discardWhitespace xorOperator
      `Combinators.chainl1` discardWhitespace andOperator
      `Combinators.chainl1` discardWhitespace orOperator

    term =
          Mega.try (Surround.parentheses expression)
      <|> Mega.try (notOperator boolean)
      <|> Mega.try (notOperator (Rumor.LooseVariable <$> Identifier.variableName))
      <|> Mega.try boolean
      <|> Mega.try equalityLoose
      <|> Rumor.LooseVariable <$> Identifier.variableName

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
-- Helpers
--------------------------------------------------------------------------------

-- Discards whitespace surrounding an operator on both sides
discardWhitespace :: Parser a -> Parser a
discardWhitespace operator = do
  Mega.try do
    space
    lexeme operator
