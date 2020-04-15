{-# LANGUAGE Safe #-}

module Rumor.Expression.Parser
( boolean
, math
, text
) where

import Rumor.Prelude
import Rumor.Expression.Type(Expression(..), simplifyText, simplifyMath)
import Data.Attoparsec.Text(Parser, char, double, skipSpace, string, takeWhile)
import qualified Data.Text as T

boolean :: Parser (Expression Bool)
boolean =
  "true"  *> (pure $ Boolean True) <|>
  "false" *> (pure $ Boolean False)

-- Number expressions
math :: Parser (Expression Double)
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
  _ <- skipSpace
  result <- parser
  _ <- skipSpace
  _ <- char ')'
  pure result

multiplicationOperator :: Parser (Expression Double -> Expression Double -> Expression Double)
multiplicationOperator =
  skipSpace *> char '*' *> skipSpace *> pure Multiply <|>
  skipSpace *> char '/' *> skipSpace *> pure Divide

additionOperator :: Parser (Expression Double -> Expression Double -> Expression Double)
additionOperator =
  skipSpace *> char '+' *> skipSpace *> pure Add <|>
  skipSpace *> char '-' *> skipSpace *> pure Subtract

number :: Parser (Expression Double)
number = Number <$> double

-- | @chainl1 p op@ parses /one/ or more occurrences of @p@,
-- separated by @op@ Returns a value obtained by a /left/ associative
-- application of all functions returned by @op@ to the values returned
-- by @p@. This parser can for example be used to eliminate left
-- recursion which typically occurs in expression grammars.
--
-- >  expr    = term   `chainl1` addop
-- >  term    = factor `chainl1` mulop
-- >  factor  = parens expr <|> integer
-- >
-- >  mulop   =   do{ symbol "*"; return (*)   }
-- >          <|> do{ symbol "/"; return (div) }
-- >
-- >  addop   =   do{ symbol "+"; return (+) }
-- >          <|> do{ symbol "-"; return (-) }
chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainl1 p op = do
  let
    go l =
      -- The operator is present
      ( do
        fn <- op
        r <- p
        go (fn l r)
      ) <|>
      -- There is no matching operator; so we just return the value
      ( pure l
      )
  l <- p
  go l

-- Text expressions
text :: Parser (Expression T.Text)
text = char '\"' *> remainingText

remainingText :: Parser (Expression T.Text)
remainingText = do
  beginning <- takeWhile (`notElem` ['{', '\\', '\"'])
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
  _ <- skipSpace
  result <-
    (BooleanSubstitution <$> boolean) <|>
    (MathSubstitution <$> number) <|>
    text
  _ <- skipSpace
  _ <- char '}'
  pure result

escape :: Parser T.Text
escape =
  string "\\n"  *> pure "\n" <|>
  string "\\r"  *> pure "\r" <|>
  string "\\{"  *> pure "{" <|>
  string "\\\"" *> pure "\"" <|>
  string "\\\\" *> pure "\\"
