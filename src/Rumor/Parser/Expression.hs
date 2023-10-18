module Rumor.Parser.Expression
( booleanExpression
, numberExpression
, stringExpression, characterEscapes, escapedChar, interpolation
) where

import Data.Scientific (Scientific)
import Data.Text (Text)
import Rumor.Parser.Common (Parser, (<?>), (<|>))

import qualified Data.Text as T
import qualified Rumor.Internal.Types as Rumor
import qualified Rumor.Parser.Lexeme as Lexeme
import qualified Rumor.Parser.Surround as Surround
import qualified Text.Megaparsec as Mega
import qualified Text.Megaparsec.Char as Char
import qualified Text.Megaparsec.Char.Lexer as Lexer
import qualified Text.Parser.Combinators as Combinators

-- $setup
-- >>> import qualified Text.Megaparsec as Mega
-- >>> let parseTest inner = Mega.parseTest (inner <* Mega.hidden Mega.eof)

--------------------------------------------------------------------------------
-- Boolean
--------------------------------------------------------------------------------

{-| Parses a boolean expression. Any amount of space, including newlines, is
  allowed between the terms of the boolean expression.

  In order from highest to lowest precedence:
  >>> parseTest booleanExpression "not true"
  LogicalNot (Boolean True)

  >>> parseTest booleanExpression "true == false"
  EqualBoolean (Boolean True) (Boolean False)

  >>> parseTest booleanExpression "true /= false"
  NotEqualBoolean (Boolean True) (Boolean False)

  >>> parseTest booleanExpression "true xor false"
  LogicalXor (Boolean True) (Boolean False)

  >>> parseTest booleanExpression "true ^ false"
  LogicalXor (Boolean True) (Boolean False)

  >>> parseTest booleanExpression "true and false"
  LogicalAnd (Boolean True) (Boolean False)

  >>> parseTest booleanExpression "true && false"
  LogicalAnd (Boolean True) (Boolean False)

  >>> parseTest booleanExpression "true or false"
  LogicalOr (Boolean True) (Boolean False)

  >>> parseTest booleanExpression "true || false"
  LogicalOr (Boolean True) (Boolean False)

  You can also do value comparisons:
  >>> parseTest booleanExpression "\"apples\" == \"oranges\""
  EqualString (String "apples") (String "oranges")

  >>> parseTest booleanExpression "1.0 /= 2.0"
  NotEqualNumber (Number 1.0) (Number 2.0)

  Examples:
  >>> parseTest booleanExpression "true and true or false xor true"
  LogicalOr (LogicalAnd (Boolean True) (Boolean True)) (LogicalXor (Boolean False) (Boolean True))

  >>> parseTest booleanExpression "true and (true or false) xor true"
  LogicalAnd (Boolean True) (LogicalXor (LogicalOr (Boolean True) (Boolean False)) (Boolean True))

  Whitespace handling:
  >>> parseTest booleanExpression "true\n||\nfalse" -- newlines are fine
  LogicalOr (Boolean True) (Boolean False)

  >>> parseTest booleanExpression "true||false" -- no space is fine
  LogicalOr (Boolean True) (Boolean False)

  >>> parseTest booleanExpression "trueorfalse" -- No space is not fine
  1:5:
    |
  1 | trueorfalse
    |     ^
  unexpected 'o'
  expecting space

  >>> parseTest booleanExpression "trueandfalse" -- No space is not fine
  1:5:
    |
  1 | trueandfalse
    |     ^
  unexpected 'a'
  expecting space

  >>> parseTest booleanExpression "truexorfalse" -- No space is not fine
  1:5:
    |
  1 | truexorfalse
    |     ^
  unexpected 'x'
  expecting space

  >>> parseTest booleanExpression "nottrue" -- No space is not fine
  1:4:
    |
  1 | nottrue
    |    ^
  unexpected 't'
  expecting space

  Error examples:
  >>> parseTest booleanExpression "/="
  1:1:
    |
  1 | /=
    | ^^
  unexpected "/="
  expecting "false", "not", "true", '!', open double quotes, open parenthesis, or signed number

  >>> parseTest booleanExpression "/= true"
  1:1:
    |
  1 | /= true
    | ^^^^^
  unexpected "/= tr"
  expecting "false", "not", "true", '!', open double quotes, open parenthesis, or signed number

  >>> parseTest booleanExpression "true /="
  1:8:
    |
  1 | true /=
    |        ^
  unexpected end of input
  expecting "false", "not", "true", '!', open double quotes, open parenthesis, or signed number
-}
booleanExpression :: Parser (Rumor.Expression Bool)
booleanExpression =
  let
    -- Parse a boolean expression with the left associative operators from
    -- highest to lowest precedence.
    expression = term
      `Combinators.chainl1` equalsOperator
      `Combinators.chainl1` notEqualsOperator
      `Combinators.chainl1` xorOperator
      `Combinators.chainl1` andOperator
      `Combinators.chainl1` orOperator

    term = Lexeme.lexeme
      (     Surround.parentheses expression
        <|> valueEquality
              stringExpression
              Rumor.EqualString
              Rumor.NotEqualString
        <|> valueEquality
              (Mega.try numberExpression <|> number)
              Rumor.EqualNumber
              Rumor.NotEqualNumber
        <|> notOperator boolean
        <|> boolean
      )

    notOperator inner = do
      _ <- Lexeme.lexeme ("!" <|> do _ <- "not"; " ")
      Rumor.LogicalNot <$> inner
    equalsOperator = do
      _ <- Lexeme.lexeme "=="
      pure Rumor.EqualBoolean
    notEqualsOperator = do
      _ <- Lexeme.lexeme ("/=" <|> "!=")
      pure Rumor.NotEqualBoolean
    xorOperator = do
      _ <- Lexeme.lexeme ("^" <|> "xor")
      pure Rumor.LogicalXor
    andOperator = do
      _ <- Lexeme.lexeme ("&&" <|> "and")
      pure Rumor.LogicalAnd
    orOperator = do
      _ <- Lexeme.lexeme ("||" <|> "or")
      pure Rumor.LogicalOr

  in
    expression

{-| Parses a value equality comparison.

  >>> import Rumor.Internal.Types (Expression(..))
  >>> let booleanEquality = valueEquality boolean EqualBoolean NotEqualBoolean

  >>> parseTest booleanEquality "true == false"
  EqualBoolean (Boolean True) (Boolean False)

  >>> parseTest booleanEquality "true/= false"
  NotEqualBoolean (Boolean True) (Boolean False)

  >>> parseTest booleanEquality "true != false"
  NotEqualBoolean (Boolean True) (Boolean False)

  >>> parseTest booleanEquality "true==false" -- No space is fine
  EqualBoolean (Boolean True) (Boolean False)

  >>> parseTest booleanEquality "true\n\n==\n\nfalse" -- Newlines are fine
  EqualBoolean (Boolean True) (Boolean False)

  >>> parseTest booleanEquality "=="
  1:1:
    |
  1 | ==
    | ^^
  unexpected "=="
  expecting "false" or "true"

  >>> parseTest booleanEquality "true =="
  1:8:
    |
  1 | true ==
    |        ^
  unexpected end of input
  expecting "false" or "true"

  >>> parseTest booleanEquality "== true"
  1:1:
    |
  1 | == true
    | ^^^^^
  unexpected "== tr"
  expecting "false" or "true"
-}
valueEquality :: Show a
              => Parser a
              -> (a -> a -> Rumor.Expression Bool)
              -> (a -> a -> Rumor.Expression Bool)
              -> Parser (Rumor.Expression Bool)
valueEquality arg eqConstructor neqConstructor = do
  l <- Lexeme.lexeme arg

  let
    equalsOperator = do
      _ <- Lexeme.lexeme "=="
      pure eqConstructor
    notEqualsOperator = do
      _ <-Lexeme.lexeme ("/=" <|> "!=")
      pure neqConstructor
  constructor <- equalsOperator <|> notEqualsOperator

  r <- arg
  pure (constructor l r)

{-| Parses a boolean literal, which can either be true or false.

  >>> parseTest boolean "true"
  Boolean True

  >>> parseTest boolean "false"
  Boolean False

  >>> parseTest boolean "True"
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

    pure result

--------------------------------------------------------------------------------
-- Number
--------------------------------------------------------------------------------

{-| Parses a mathematical expression. Any amount of space, including newlines,
  is allowed between the terms of the mathematical expression.

  In order from highest to lowest precedence:
  >>> parseTest numberExpression "1 * 2"
  Multiplication (Number 1.0) (Number 2.0)

  >>> parseTest numberExpression "1 / 2"
  Division (Number 1.0) (Number 2.0)

  >>> parseTest numberExpression "1 + 2"
  Addition (Number 1.0) (Number 2.0)

  >>> parseTest numberExpression "1 - 2"
  Subtraction (Number 1.0) (Number 2.0)

  You can use negative numbers too:
  >>> parseTest numberExpression "-1 - -2"
  Subtraction (Number (-1.0)) (Number (-2.0))

  Examples:
  >>> parseTest numberExpression "1"
  Number 1.0

  >>> parseTest numberExpression "1 * 2 + 3 / -4"
  Addition (Multiplication (Number 1.0) (Number 2.0)) (Division (Number 3.0) (Number (-4.0)))

  >>> parseTest numberExpression "1 * (2 + 3) / -4"
  Division (Multiplication (Number 1.0) (Addition (Number 2.0) (Number 3.0))) (Number (-4.0))

  Whitespace handling:
  >>> parseTest numberExpression "1*3--4/2" -- No whitespace is okay
  Subtraction (Multiplication (Number 1.0) (Number 3.0)) (Division (Number (-4.0)) (Number 2.0))

  >>> parseTest numberExpression "1\n*\n3\n-\n-4\n/\n2" -- Newlines are okay
  Subtraction (Multiplication (Number 1.0) (Number 3.0)) (Division (Number (-4.0)) (Number 2.0))

  Error examples:
  >>> parseTest numberExpression "*"
  1:1:
    |
  1 | *
    | ^
  unexpected '*'
  expecting open parenthesis or signed number

  >>> parseTest numberExpression "1 * 2 + 3 /"
  1:12:
    |
  1 | 1 * 2 + 3 /
    |            ^
  unexpected end of input
  expecting open parenthesis or signed number

  >>> parseTest numberExpression "1 * 2 + 3 /= 4"
  1:12:
    |
  1 | 1 * 2 + 3 /= 4
    |            ^
  unexpected '='
-}
numberExpression :: Parser (Rumor.Expression Scientific)
numberExpression =
  let
    expr = term
      `Combinators.chainl1` multiplicationOperator
      `Combinators.chainl1` divisionOperator
      `Combinators.chainl1` additionOperator
      `Combinators.chainl1` subtractionOperator
    term = Lexeme.lexeme
      (     Surround.parentheses expr
        <|> number
      )

    multiplicationOperator = do
      _ <- Lexeme.lexeme (Char.char '*')
      pure Rumor.Multiplication
    divisionOperator = do
      Lexeme.lexeme do
        _ <- Char.char '/'
        -- Ensure this isn't a `/=` operator
        Mega.notFollowedBy (Char.char '=')
      pure Rumor.Division
    additionOperator = do
      _ <- Lexeme.lexeme (Char.char '+')
      pure Rumor.Addition
    subtractionOperator = do
      _ <- Lexeme.lexeme (Char.char '-')
      pure Rumor.Subtraction

  in
    expr <|> number

{-| Parses a number literal, which can be any signed decimal number. It can be
  written using scientific notation.

  >>> parseTest number "1.0"
  Number 1.0

  >>> parseTest number "+1.0"
  Number 1.0

  >>> parseTest number "-1.0"
  Number (-1.0)

  >>> parseTest number "+1"
  Number 1.0

  >>> parseTest number "-1"
  Number (-1.0)

  >>> parseTest number "1e10"
  Number 1.0e10

  >>> parseTest number "+1e10"
  Number 1.0e10

  >>> parseTest number "-1e10"
  Number (-1.0e10)

  Error examples:
  >>> parseTest number "1."
  1:2:
    |
  1 | 1.
    |  ^
  unexpected '.'
  expecting 'E', 'e', or digit

  >>> parseTest number ".1"
  1:1:
    |
  1 | .1
    | ^
  unexpected '.'
  expecting signed number
-}
number :: Parser (Rumor.Expression Scientific)
number = do
  n <- Lexer.signed Lexeme.hspace Lexer.scientific <?> "signed number"
  pure (Rumor.Number n)

--------------------------------------------------------------------------------
-- Text
--------------------------------------------------------------------------------

{-| Parses a string expression, which is any quoted string with interpolated
  values and no newlines.

  Examples:
  >>> parseTest stringExpression "\"Hello world!\""
  String "Hello world!"

  >>> parseTest stringExpression "\"I have { 5 } mangoes!\""
  Concat (String "I have ") (Concat (NumberToString (Number 5.0)) (String " mangoes!"))

  >>> parseTest stringExpression "\"Hello\\nworld!\""
  Concat (String "Hello") (Concat (String "\n") (String "world!"))

  Whitespace handling:
  >>> parseTest stringExpression "\"Hello\nworld!\"" -- Newlines are not okay
  1:7:
    |
  1 | "Hello
    |       ^
  unexpected newline
  expecting '\', close double quotes, interpolation, or literal char

  Error examples:
  >>> parseTest stringExpression "\""
  1:2:
    |
  1 | "
    |  ^
  unexpected end of input
  expecting '\', close double quotes, interpolation, or literal char

  >>> parseTest stringExpression "\"Hello world!"
  1:14:
    |
  1 | "Hello world!
    |              ^
  unexpected end of input
  expecting '\', close double quotes, interpolation, or literal char

  >>> parseTest stringExpression "Hello world!\""
  1:1:
    |
  1 | Hello world!"
    | ^
  unexpected 'H'
  expecting open double quotes

  >>> parseTest stringExpression "\"}\"" -- You must escape the close bracket
  1:2:
    |
  1 | "}"
    |  ^
  unexpected '}'
  expecting '\', close double quotes, interpolation, or literal char
-}
stringExpression :: Parser (Rumor.Expression Text)
stringExpression = do
  let
    literalString = do
      string <-
        Mega.takeWhile1P
          (Just "literal char")
          (`notElem` (fst <$> characterEscapes))
      pure (Rumor.String string)

  Surround.doubleQuotes do
    texts <- Mega.many
      (     Mega.try literalString
        <|> Mega.try escapedChar
        <|> Mega.try interpolation
      )
    pure (mconcat texts)

{-| The characters that can be escaped, and their corresponding escape code.
-}
characterEscapes :: [(Char, Text)]
characterEscapes =
  [ ('\n', "n")
  , ('\r', "r")
  , ('\\', "\\")
  , ('{', "{")
  , ('}', "}")
  , ('"', "\"")
  ]

{-| Parses a character escape sequence.

  >>> parseTest escapedChar "\\n"
  String "\n"

  >>> parseTest escapedChar "\\r"
  String "\r"

  >>> parseTest escapedChar "\\\\"
  String "\\"

  >>> parseTest escapedChar "\\{"
  String "{"

  >>> parseTest escapedChar "\\}"
  String "}"

  >>> parseTest escapedChar "\\\""
  String "\""

  >>> parseTest escapedChar "\\"
  1:2:
    |
  1 | \
    |  ^
  unexpected end of input
  expecting '"', '\', 'n', 'r', '{', or '}'
-}
escapedChar :: Parser (Rumor.Expression Text)
escapedChar = do
  let
    escape (ch, escapeCode) = do
      _ <- Char.string escapeCode
      pure (Rumor.String (T.singleton ch))

  _ <- Char.char '\\'
  Mega.choice (escape <$> characterEscapes)

{-| Parses a string interpolation, which is a boolean, number, or string
  expression surrounded by braces.

  Examples:
  >>> parseTest interpolation "{ true }"
  BooleanToString (Boolean True)

  >>> parseTest interpolation "{ 123 }"
  NumberToString (Number 123.0)

  >>> parseTest interpolation "{ \"foobar\" }"
  String "foobar"

  Whitespace handling:
  >>> parseTest interpolation "{true}" -- No whitespace is okay
  BooleanToString (Boolean True)

  >>> parseTest interpolation "{\ntrue\n}" -- Newlines are okay
  BooleanToString (Boolean True)

  Error examples:
  >>> parseTest interpolation "{}"
  1:2:
    |
  1 | {}
    |  ^
  unexpected '}'
  expecting "false", "not", "true", '!', open double quotes, open parenthesis, or signed number

  >>> parseTest interpolation "{true"
  1:6:
    |
  1 | {true
    |      ^
  unexpected end of input
  expecting "!=", "&&", "/=", "==", "and", "or", "xor", "||", '^', or close brace

  >>> parseTest interpolation "true}"
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
    (     Mega.try (Rumor.BooleanToString <$> booleanExpression)
      <|> Mega.try (Rumor.NumberToString <$> numberExpression)
      <|> stringExpression
    ) <?> "interpolation"
