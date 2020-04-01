{-# LANGUAGE GADTs #-}
{-# LANGUAGE Safe #-}

module Rumor.Expression.Parser
( boolean
, number
, text
) where

import Rumor.Prelude
import Rumor.Expression.Type(Expression(..), simplifyText)
import Data.Attoparsec.Text(Parser, char, double, skipSpace, string, takeWhile)
import qualified Data.Text as T

boolean :: Parser (Expression Bool)
boolean =
  "true"  *> (pure $ Boolean True) <|>
  "false" *> (pure $ Boolean False)

number :: Parser (Expression Double)
number = Number <$> double

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
    (NumberSubstitution <$> number) <|>
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
