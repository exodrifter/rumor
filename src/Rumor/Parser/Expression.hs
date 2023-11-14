module Rumor.Parser.Expression
( anyExpression
, escape, interpolation, stringEscapes
) where

import Data.Text (Text)
import Rumor.Parser.Common (Parser, hspace, lexeme, space, (<|>), (<?>))

import qualified Data.Text as T
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
-- Writing annotation information for tests that succeed is tedious and not
-- very useful. We only want to check that the annotations are correct in error
-- messages.
-- >>> let parse inner = parseTest newContext (unAnnotate <$> (inner <* eof))

anyExpression :: Parser Rumor.AnnotatedExpression
anyExpression =
      booleanExpression
  <|> numberExpression
  <|> stringExpression


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
booleanExpression :: Parser Rumor.AnnotatedExpression
booleanExpression =
  let
    -- Parse a boolean expression with the left associative operators from
    -- highest to lowest precedence.
    go = factor
      `Combinators.chainl1` discardWhitespace eqOperator
      `Combinators.chainl1` discardWhitespace neqOperator
      `Combinators.chainl1` discardWhitespace xorOperator
      `Combinators.chainl1` discardWhitespace andOperator
      `Combinators.chainl1` discardWhitespace orOperator

    factor =
          Mega.try (Surround.parentheses go)
      <|> Mega.try (notOperator term)
      <|> Mega.try equalityExpression
      <|> term

    term =
          Mega.try boolean
      <|> variable

    notOperator inner = discardWhitespace do
      begin <- Mega.getOffset
      _ <- "!" <|> do _ <- "not"; " "
      end <- Mega.getOffset
      Rumor.AnnotatedLogicalNot begin end <$> inner
    eqOperator = do
      begin <- Mega.getOffset
      _ <- "==" <|> "is"
      end <- Mega.getOffset
      pure (Rumor.AnnotatedEqual begin end)
    neqOperator = do
      begin <- Mega.getOffset
      _ <- "/="
      end <- Mega.getOffset
      pure (Rumor.AnnotatedNotEqual begin end)
    xorOperator = do
      begin <- Mega.getOffset
      _ <- "^" <|> "xor"
      end <- Mega.getOffset
      pure (Rumor.AnnotatedLogicalXor begin end)
    andOperator = do
      begin <- Mega.getOffset
      _ <- "&&" <|> "and"
      end <- Mega.getOffset
      pure (Rumor.AnnotatedLogicalAnd begin end)
    orOperator = do
      begin <- Mega.getOffset
      _ <- "||" <|> "or"
      end <- Mega.getOffset
      pure (Rumor.AnnotatedLogicalOr begin end)

  in
    go

boolean :: Parser Rumor.AnnotatedExpression
boolean =
  let
    true = do
      begin <- Mega.getOffset
      _ <- "true"
      end <- Mega.getOffset
      pure (Rumor.AnnotatedBoolean begin end True)
    false = do
      begin <- Mega.getOffset
      _ <- "false"
      end <- Mega.getOffset
      pure (Rumor.AnnotatedBoolean begin end False)

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
equalityExpression :: Parser Rumor.AnnotatedExpression
equalityExpression = do
  let
    term =
          Mega.try (Surround.parentheses equalityExpression)
      <|> Mega.try number
      <|> variable

    eqOperator = do
      begin <- Mega.getOffset
      _ <- "==" <|> "is"
      end <- Mega.getOffset
      pure (Rumor.AnnotatedEqual begin end)
    neqOperator = do
      begin <- Mega.getOffset
      _ <- "/="
      end <- Mega.getOffset
      pure (Rumor.AnnotatedNotEqual begin end)

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
numberExpression :: Parser Rumor.AnnotatedExpression
numberExpression =
  let
    go = term
      `Combinators.chainl1` discardWhitespace multiplicationOperator
      `Combinators.chainl1` divisionOperator
      `Combinators.chainl1` discardWhitespace additionOperator
      `Combinators.chainl1` discardWhitespace subtractionOperator
    term =
          Surround.parentheses go
      <|> number
      <|> variable

    multiplicationOperator = do
      begin <- Mega.getOffset
      _ <- Char.char '*'
      end <- Mega.getOffset
      pure (Rumor.AnnotatedMultiplication begin end)
    divisionOperator = do
      (begin, end) <- Mega.try do
        space
        b <- Mega.getOffset
        _ <- Char.char '/'
        e <- Mega.getOffset
        pure (b, e)

      -- Ensure this isn't a `/=` operator
      Mega.notFollowedBy (Char.char '=')
      space
      pure (Rumor.AnnotatedDivision begin end)
    additionOperator = do
      begin <- Mega.getOffset
      _ <- Char.char '+'
      end <- Mega.getOffset
      pure (Rumor.AnnotatedAddition begin end)
    subtractionOperator = do
      begin <- Mega.getOffset
      _ <- Char.char '-'
      end <- Mega.getOffset
      pure (Rumor.AnnotatedSubtraction begin end)

  in
    go

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
number :: Parser Rumor.AnnotatedExpression
number = do
  begin <- Mega.getOffset
  n <- Lexer.signed hspace Lexer.scientific <?> "signed number"
  end <- Mega.getOffset
  pure (Rumor.AnnotatedNumber begin end n)

--------------------------------------------------------------------------------
-- String
--------------------------------------------------------------------------------

stringExpression :: Parser Rumor.AnnotatedExpression
stringExpression =
      string
  <|> variable

{-| Parses a string, which is any quoted string with interpolated values and no
  newlines.

  >>> parse stringExpression "\"Hello world!\""
  String "Hello world!"

  >>> parse stringExpression "\"I have { 5 } mangoes!\""
  Concat (String "I have ") (Concat (ToString (Number 5.0)) (String " mangoes!"))

  >>> parse stringExpression "\"Hello\\nworld!\""
  Concat (String "Hello") (Concat (String "\n") (String "world!"))

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
  expecting end of input or variable character

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
string :: Parser Rumor.AnnotatedExpression
string = do
  let
    literalString = do
      begin <- Mega.getOffset
      text <-
        Mega.takeWhile1P
          (Just "literal char")
          (`notElem` (fst <$> stringEscapes))
      end <- Mega.getOffset
      pure (Rumor.AnnotatedString begin end text)

  begin <- Mega.getOffset
  Surround.doubleQuotes do
    texts <- Mega.many
      (     Mega.try literalString
        <|> Mega.try (escape stringEscapes)
        <|> interpolation
      )
    end <- Mega.getOffset
    pure (Rumor.concatAnnotatedStrings begin end texts)

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
escape :: [(Char, Text)] -> Parser Rumor.AnnotatedExpression
escape escapes = do
  let
    escapeParser begin (ch, escapeCode) = do
      _ <- Char.string escapeCode
      end <- Mega.getOffset
      pure (Rumor.AnnotatedString begin end (T.singleton ch))

  b <- Mega.getOffset
  _ <- Char.char '\\'
  Mega.choice (escapeParser b <$> escapes)

{-| Parses a string interpolation, which is a boolean, number, or string
  expression surrounded by braces.

  Examples:
  >>> parse interpolation "{ true }"
  ToString (Boolean True)

  >>> parse interpolation "{ true || false }"
  ToString (LogicalOr (Boolean True) (Boolean False))

  >>> parse interpolation "{ 123 }"
  ToString (Number 123.0)

  >>> parse interpolation "{ 123 + 456 }"
  ToString (Addition (Number 123.0) (Number 456.0))

  >>> parse interpolation "{ \"foobar\" }"
  ToString (String "foobar")

  You can use whitespace between the braces and the expression.

  >>> parse interpolation "{true}" -- No whitespace is okay
  ToString (Boolean True)

  >>> parse interpolation "{\ntrue\n}" -- Newlines are okay
  ToString (Boolean True)

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
  expecting "&&", "/=", "==", "and", "is", "or", "xor", "||", '^', or close brace

  >>> parse interpolation "true}"
  1:1:
    |
  1 | true}
    | ^
  unexpected 't'
  expecting interpolation
-}
interpolation :: Parser Rumor.AnnotatedExpression
interpolation = do
  let toString innerExpression = do
        inner <- innerExpression
        pure (\b e -> Rumor.AnnotatedToString b e inner)
  begin <- Mega.getOffset
  result <- Surround.braces
    (     Mega.try (toString (lexeme booleanExpression))
      <|> Mega.try (toString (lexeme numberExpression))
      <|> lexeme (toString stringExpression)
    ) <?> "interpolation"
  end <- Mega.getOffset
  pure (result begin end)

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

variable :: Parser Rumor.AnnotatedExpression
variable = do
  begin <- Mega.getOffset
  name <- Identifier.variableName
  end <- Mega.getOffset
  pure (Rumor.AnnotatedVariable begin end name)

-- Discards whitespace surrounding an operator on both sides
discardWhitespace :: Parser a -> Parser a
discardWhitespace operator = do
  Mega.try do
    space
    lexeme operator
