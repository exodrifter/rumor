{-# LANGUAGE Safe #-}

module Rumor.Expression.Parser
( boolean
, math
, text
) where

import Rumor.Parser
import Rumor.Prelude
import Rumor.Expression.Type(Expression(..), simplifyText, simplifyMath)
import qualified Data.Text as T

boolean :: Parser (Expression Bool)
boolean =
  string "true"  *> (pure $ Boolean True) <|>
  string "false" *> (pure $ Boolean False)

-- Number expressions
math :: Parser (Expression Pico)
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

parenthesis :: Parser (Expression a) -> Parser (Expression a)
parenthesis parser = do
  _ <- char '('
  _ <- spaces
  result <- parser
  _ <- spaces
  _ <- char ')'
  pure result

multiplicationOperator :: Parser (Expression Pico -> Expression Pico -> Expression Pico)
multiplicationOperator =
  spaces *> char '*' *> spaces *> pure Multiply <|>
  spaces *> char '/' *> spaces *> pure Divide

additionOperator :: Parser (Expression Pico -> Expression Pico -> Expression Pico)
additionOperator =
  spaces *> char '+' *> spaces *> pure Add <|>
  spaces *> char '-' *> spaces *> pure Subtract

number :: Parser (Expression Pico)
number = Number <$> fixed

-- Text expressions
text :: Parser (Expression T.Text)
text = char '\"' *> remainingText

remainingText :: Parser (Expression T.Text)
remainingText = do
  beginning <- T.pack <$> manyTill anyChar (oneOf ['{', '\\', '\"'])
  result <-
      ( do -- Substitution sequence
          sub <- substitution
          rest <- remainingText
          pure $ Concat (Concat (Text beginning) sub) rest
      ) <|>
      ( do -- Escape sequence
          esc <- escape
          rest <- remainingText
          pure $ Concat (Text (beginning <> esc)) rest
      ) <|>
      ( do -- Normal text
          _ <- char '\"'
          pure $ Text beginning
      )
  pure $ simplifyText result

substitution :: Parser (Expression T.Text)
substitution = do
  _ <- char '{'
  _ <- spaces
  result <-
    BooleanSubstitution <$> boolean <|>
    MathSubstitution <$> math <|>
    text
  _ <- spaces
  _ <- char '}'
  pure result

escape :: Parser T.Text
escape =
  string "\\n"  *> pure "\n" <|>
  string "\\r"  *> pure "\r" <|>
  string "\\{"  *> pure "{"  <|>
  string "\\\"" *> pure "\"" <|>
  string "\\\\" *> pure "\\"
