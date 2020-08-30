module Rumor.Expression.Parser
( boolean
, math
, text
) where

import Rumor.Expression.Type
import Rumor.Parser

import qualified Data.Text as T

boolean :: Parser (Expression r Bool)
boolean =
  string "true"  *> (pure $ Boolean True) <|>
  string "false" *> (pure $ Boolean False)

-- Number expressions
math :: HasResolution r => Parser (Expression r (Fixed r))
math =
  let piece = parenthesis math <|> number
      -- We chain the multiplication and division operators first to
      -- give them a higher precedence than the subtraction and addition
      -- operators.
      chainedMultiplication = chainl1 piece multiplicationOperator
      -- Now that all of the groups of multiplication and division operators
      -- have been taken care of, we can add all of the multiplied sections
      -- together
      chainedAddition = chainl1 chainedMultiplication additionOperator
  in  simplifyMath <$> chainedAddition

parenthesis :: Parser (Expression r a) -> Parser (Expression r a)
parenthesis parser = do
  _ <- char '('
  spaces
  result <- parser
  spaces
  _ <- char ')'
  pure result

type Operator r a = Expression r a -> Expression r a -> Expression r a

multiplicationOperator :: Parser (Operator r (Fixed r))
multiplicationOperator =
  spaces *> char '*' *> spaces *> pure Multiply <|>
  spaces *> char '/' *> spaces *> pure Divide

additionOperator :: Parser (Operator r (Fixed r))
additionOperator =
  spaces *> char '+' *> spaces *> pure Add <|>
  spaces *> char '-' *> spaces *> pure Subtract

number :: HasResolution r => Parser (Expression r (Fixed r))
number = Number <$> fixed

-- Text expressions
quote :: HasResolution r => Parser (Expression r T.Text)
quote = simplifyText <$> (char '\"' *> remainingQuote)

remainingQuote :: HasResolution r => Parser (Expression r T.Text)
remainingQuote = do
  beginning <- T.pack <$> manyTill anyChar (oneOf ['{', '\\', '\"'])
  result <-
      ( do -- Substitution sequence
          sub <- substitution
          rest <- remainingQuote
          pure $ Concat (Concat (Text beginning) sub) rest
      ) <|>
      ( do -- Escape sequence
          esc <- escape
          rest <- remainingQuote
          pure $ Concat (Text (beginning <> esc)) rest
      ) <|>
      ( do -- Normal text
          _ <- char '\"'
          pure $ Text beginning
      )
  pure result

text :: HasResolution r => Parser (Expression r T.Text)
text = withPos $ simplifyText <$> remainingText

remainingText :: HasResolution r => Parser (Expression r T.Text)
remainingText = do
  beginning <- T.pack <$>
    manyTill anyChar (void (char '{') <|> eol <|> eof)
  result <-
      ( do -- Substitution sequence
          sub <- substitution
          rest <- remainingText
          pure $ Concat (Concat (Text beginning) sub) rest
      ) <|>
      ( do -- End of line with more content
          eol
          s <- spaces *> pure " "
          sameOrIndented
          Concat (Text $ s <> beginning) <$> remainingText
      ) <|>
      ( do -- End of line with no more content
          eol
          pure $ Text beginning
      ) <|>
      ( do -- End of file
          pure $ Text beginning
      )
  pure result

substitution :: HasResolution r => Parser (Expression r T.Text)
substitution = do
  _ <- char '{'
  spaces
  result <-
    BooleanSubstitution <$> boolean <|>
    MathSubstitution <$> math <|>
    quote
  spaces
  _ <- char '}'
  pure result

escape :: Parser T.Text
escape =
  string "\\n"  *> pure "\n" <|>
  string "\\r"  *> pure "\r" <|>
  string "\\{"  *> pure "{"  <|>
  string "\\\"" *> pure "\"" <|>
  string "\\\\" *> pure "\\"
