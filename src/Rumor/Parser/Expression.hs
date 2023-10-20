module Rumor.Parser.Expression
( booleanExpression
, numberExpression
, stringExpression, stringEscapes, escape, interpolation
) where

import Data.Char (isLetter, isMark, isDigit)
import Data.Scientific (Scientific)
import Data.Text (Text)
import Rumor.Parser.Common (Parser, rumorError, hspace, lexeme, space, (<?>), (<|>))

import qualified Data.NonEmptyText as NET
import qualified Data.Text as T
import qualified Rumor.Internal.Types as Rumor
import qualified Rumor.Parser.Surround as Surround
import qualified Text.Megaparsec as Mega
import qualified Text.Megaparsec.Char as Char
import qualified Text.Megaparsec.Char.Lexer as Lexer
import qualified Text.Parser.Combinators as Combinators

-- $setup
-- >>> import Rumor.Parser.Common
-- >>> import Rumor.Internal.Types (Expression(..))
-- >>> let parse inner = parseTest newContext (inner <* eof)

--------------------------------------------------------------------------------
-- Boolean
--------------------------------------------------------------------------------

{-| Parses a boolean expression. Any amount of space, including newlines, is
  allowed between the terms of the boolean expression.

  >>> parse booleanExpression "true"
  Boolean True

  >>> parse booleanExpression "false"
  Boolean False

  In order from highest to lowest precedence:

  >>> parse booleanExpression "not true"
  LogicalNot (Boolean True)

  >>> parse booleanExpression "true == false"
  EqualBoolean (Boolean True) (Boolean False)

  >>> parse booleanExpression "true is false"
  EqualBoolean (Boolean True) (Boolean False)

  >>> parse booleanExpression "true /= false"
  NotEqualBoolean (Boolean True) (Boolean False)

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

  You can also do value comparisons.

  >>> parse booleanExpression "\"apples\" == \"oranges\""
  EqualString (String "apples") (String "oranges")

  >>> parse booleanExpression "1.0 /= 2.0"
  NotEqualNumber (Number 1.0) (Number 2.0)

  You can also use variables.

  >>> parse booleanExpression "foobar"
  BooleanVariable (VariableName (Unicode "foobar"))

  >>> parse booleanExpression "foobar || true"
  LogicalOr (BooleanVariable (VariableName (Unicode "foobar"))) (Boolean True)

  TODO: I'm not sure how to handle this case yet, I don't think it's a good idea
  to treat all variables as booleans when an equality is involved.
  >>> parse booleanExpression "foo == bar"
  EqualBoolean (BooleanVariable (VariableName (Unicode "foo"))) (BooleanVariable (VariableName (Unicode "bar")))

  You can use newlines.

  >>> parse booleanExpression "true\n||\nfalse"
  LogicalOr (Boolean True) (Boolean False)

  You don't need to use whitespace if you are using the non-word forms of the
  operators.

  >>> parse booleanExpression "true||false"
  LogicalOr (Boolean True) (Boolean False)

  >>> parse booleanExpression "trueorfalse"
  BooleanVariable (VariableName (Unicode "trueorfalse"))

  >>> parse booleanExpression "true&&false"
  LogicalAnd (Boolean True) (Boolean False)

  >>> parse booleanExpression "trueandfalse"
  BooleanVariable (VariableName (Unicode "trueandfalse"))

  >>> parse booleanExpression "true^false"
  LogicalXor (Boolean True) (Boolean False)

  >>> parse booleanExpression "truexorfalse"
  BooleanVariable (VariableName (Unicode "truexorfalse"))

  >>> parse booleanExpression "!false"
  LogicalNot (Boolean False)

  >>> parse booleanExpression "nottrue"
  BooleanVariable (VariableName (Unicode "nottrue"))

  >>> parse booleanExpression "trueistrue"
  BooleanVariable (VariableName (Unicode "trueistrue"))

  The same applies for variables, though with variables you will just end up
  with a different variable name.

  >>> parse booleanExpression "foo||bar"
  LogicalOr (BooleanVariable (VariableName (Unicode "foo"))) (BooleanVariable (VariableName (Unicode "bar")))

  >>> parse booleanExpression "fooorbar"
  BooleanVariable (VariableName (Unicode "fooorbar"))

  >>> parse booleanExpression "foo&&bar"
  LogicalAnd (BooleanVariable (VariableName (Unicode "foo"))) (BooleanVariable (VariableName (Unicode "bar")))

  >>> parse booleanExpression "fooandbar"
  BooleanVariable (VariableName (Unicode "fooandbar"))

  >>> parse booleanExpression "foo^bar"
  LogicalXor (BooleanVariable (VariableName (Unicode "foo"))) (BooleanVariable (VariableName (Unicode "bar")))

  >>> parse booleanExpression "fooxorbar"
  BooleanVariable (VariableName (Unicode "fooxorbar"))

  >>> parse booleanExpression "!bar"
  LogicalNot (BooleanVariable (VariableName (Unicode "bar")))

  >>> parse booleanExpression "notfoo"
  BooleanVariable (VariableName (Unicode "notfoo"))

  >>> parse booleanExpression "fooistrue"
  BooleanVariable (VariableName (Unicode "fooistrue"))

  You cannot write incomplete boolean expressions.

  >>> parse booleanExpression "/="
  1:1:
    |
  1 | /=
    | ^^
  unexpected "/="
  expecting "false", "not", "true", '!', open double quotes, open parenthesis, signed number, or variable

  >>> parse booleanExpression "/= true"
  1:1:
    |
  1 | /= true
    | ^^^^^
  unexpected "/= tr"
  expecting "false", "not", "true", '!', open double quotes, open parenthesis, signed number, or variable

  >>> parse booleanExpression "true /="
  1:8:
    |
  1 | true /=
    |        ^
  unexpected end of input
  expecting "false", "not", "true", '!', open double quotes, open parenthesis, signed number, or variable

  This parser doesn't consume trailing whitespace.

  >>> parse booleanExpression "true  \n  "
  1:5:
    |
  1 | true
    |     ^
  unexpected space
  expecting end of input
-}
booleanExpression :: Parser (Rumor.Expression Bool)
booleanExpression =
  let
    -- Parse a boolean expression with the left associative operators from
    -- highest to lowest precedence.
    expression = term
      `Combinators.chainl1` discardWhitespace equalsOperator
      `Combinators.chainl1` discardWhitespace notEqualsOperator
      `Combinators.chainl1` discardWhitespace xorOperator
      `Combinators.chainl1` discardWhitespace andOperator
      `Combinators.chainl1` discardWhitespace orOperator

    term =
          Surround.parentheses expression
      <|> Mega.try (variable Rumor.BooleanVariable)
      <|> valueEquality
            stringExpression
            Rumor.EqualString
            Rumor.NotEqualString
      <|> valueEquality
            (Mega.try numberExpression <|> number <|> variable Rumor.NumberVariable)
            Rumor.EqualNumber
            Rumor.NotEqualNumber
      <|> Mega.try (notOperator boolean)
      <|> notOperator (variable Rumor.BooleanVariable)
      <|> boolean

    notOperator inner = do
      _ <- discardWhitespace ("!" <|> do _ <- "not"; " ")
      Rumor.LogicalNot <$> inner
    equalsOperator = do
      _ <- "==" <|> "is"
      pure Rumor.EqualBoolean
    notEqualsOperator = do
      _ <- "/=" <|> "!="
      pure Rumor.NotEqualBoolean
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

{-| Parses a value equality comparison.

  >>> let booleanEquality = valueEquality boolean EqualBoolean NotEqualBoolean
  >>> parse booleanEquality "true == false"
  EqualBoolean (Boolean True) (Boolean False)

  >>> parse booleanEquality "true /= false"
  NotEqualBoolean (Boolean True) (Boolean False)

  >>> parse booleanEquality "true != false"
  NotEqualBoolean (Boolean True) (Boolean False)

  You can use newlines.

  >>> parse booleanEquality "true\n\n==\n\nfalse" -- Newlines are fine
  EqualBoolean (Boolean True) (Boolean False)

  You don't need to use whitespace if you are using the non-word forms of the
  operators.

  >>> parse booleanEquality "true==false"
  EqualBoolean (Boolean True) (Boolean False)

  You cannot write incomplete equality expressions.

  >>> parse booleanEquality "=="
  1:1:
    |
  1 | ==
    | ^^
  unexpected "=="
  expecting "false" or "true"

  >>> parse booleanEquality "true =="
  1:8:
    |
  1 | true ==
    |        ^
  unexpected end of input
  expecting "false" or "true"

  >>> parse booleanEquality "== true"
  1:1:
    |
  1 | == true
    | ^^^^^
  unexpected "== tr"
  expecting "false" or "true"

  This parser doesn't consume trailing whitespace.

  >>> parse booleanExpression "true == false  \n  "
  1:14:
    |
  1 | true == false
    |              ^
  unexpected space
  expecting end of input
-}
valueEquality :: Show a
              => Parser a
              -> (a -> a -> Rumor.Expression Bool)
              -> (a -> a -> Rumor.Expression Bool)
              -> Parser (Rumor.Expression Bool)
valueEquality arg eqConstructor neqConstructor = do
  l <- lexeme arg

  let
    equalsOperator = do
      _ <- lexeme ("==" <|> "is")
      pure eqConstructor
    notEqualsOperator = do
      _ <-lexeme ("/=" <|> "!=")
      pure neqConstructor
  constructor <- equalsOperator <|> notEqualsOperator

  r <- arg
  pure (constructor l r)

{-| Parses a boolean literal, which can either be true or false.

  >>> parse boolean "true"
  Boolean True

  >>> parse boolean "false"
  Boolean False

  >>> parse boolean "True"
  1:1:
    |
  1 | True
    | ^^^^
  unexpected "True"
  expecting "false" or "true"
-}
boolean :: Parser (Rumor.Expression Bool)
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
  NumberVariable (VariableName (Unicode "foobar"))

  >>> parse numberExpression "foobar + 1.0"
  Addition (NumberVariable (VariableName (Unicode "foobar"))) (Number 1.0)

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
  expecting open parenthesis, signed number, or variable

  >>> parse numberExpression "1 * 2 + 3 /"
  1:12:
    |
  1 | 1 * 2 + 3 /
    |            ^
  unexpected end of input
  expecting open parenthesis, signed number, or variable

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
numberExpression :: Parser (Rumor.Expression Scientific)
numberExpression =
  let
    expr = term
      `Combinators.chainl1` discardWhitespace multiplicationOperator
      `Combinators.chainl1` divisionOperator
      `Combinators.chainl1` discardWhitespace additionOperator
      `Combinators.chainl1` discardWhitespace subtractionOperator
    term =
          Surround.parentheses expr
      <|> number
      <|> variable Rumor.NumberVariable

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
    expr <|> number

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
number :: Parser (Rumor.Expression Scientific)
number = do
  n <- Lexer.signed hspace Lexer.scientific <?> "signed number"
  pure (Rumor.Number n)

--------------------------------------------------------------------------------
-- Text
--------------------------------------------------------------------------------

stringExpression :: Parser (Rumor.Expression Text)
stringExpression =
      string
  <|> variable Rumor.StringVariable

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
  expecting "false", "not", "true", '!', open double quotes, open parenthesis, signed number, or variable

  You must provide both braces for an interpolation.

  >>> parse interpolation "{true"
  1:6:
    |
  1 | {true
    |      ^
  unexpected end of input
  expecting "!=", "&&", "/=", "==", "and", "is", "or", "xor", "||", '^', or close brace

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

{-| Parses the name of a variable. A variable is any identifier that doesn't
  start with a number and isn't a reserved keyword.

  >>> parse (variable Rumor.StringVariable) "foobar"
  StringVariable (VariableName (Unicode "foobar"))

  >>> parse (variable Rumor.StringVariable) "foo123"
  StringVariable (VariableName (Unicode "foo123"))

  >>> parse (variable Rumor.StringVariable) "123foo"
  1:1:
    |
  1 | 123foo
    | ^
  unexpected '1'
  expecting variable

  >>> parse (variable Rumor.StringVariable) "true"
  1:1:
    |
  1 | true
    | ^^^^
  Cannot use true as a variable name
-}
variable :: (Rumor.VariableName -> Rumor.Expression a) -> Parser (Rumor.Expression a)
variable constructor =
  let
    validFirst ch =
         isLetter ch
      || isMark ch
      || ch == '-'
      || ch == '_'
    validRest ch =
         validFirst ch
      || isDigit ch
    reservedKeywords =
      [ NET.new 't' "rue"
      , NET.new 'f' "alse"
      , NET.new 'n' "ot"
      , NET.new 'a' "nd"
      , NET.new 'o' "r"
      , NET.new 'x' "or"
      , NET.new 'i' "s"
      ]

    parser = do
      first <- Mega.satisfy validFirst <?> "non-digit variable character"
      rest <- Mega.takeWhileP (Just "variable character") validRest
      pure (NET.new first rest)

  in do
    name <- Mega.lookAhead (lexeme parser) <?> "variable"
    if name `elem` reservedKeywords
    then rumorError
            ("Cannot use " <> NET.toText name <> " as a variable name")
            (NET.length name)
    else do
      _ <- Mega.takeP (Just "variable character") (NET.length name)
      pure (constructor (Rumor.VariableName (Rumor.Unicode name)))

-- Discards whitespace surrounding an operator on both sides
discardWhitespace :: Parser a -> Parser a
discardWhitespace operator = do
  Mega.try do
    space
    lexeme operator
