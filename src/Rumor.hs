module Rumor where

import Data.NonEmptyText (NonEmptyText)
import Text.Megaparsec ((<?>), (<|>))
import Rumor.Parser
  ( Parser
  , hlexeme
  , identifier
  , lexeme
  , stringExpression
  , nodes
  )

import qualified Data.Text as T
import qualified Text.Megaparsec as Mega
import qualified Text.Megaparsec.Char as Char
import qualified Text.Megaparsec.Error as Error
import qualified Rumor.Internal.Types as Rumor

parse :: String -> T.Text -> Either String [Rumor.Node]
parse fileName fileContents =
  case Mega.runParser nodes fileName fileContents of
    Right result -> Right result
    Left err -> Left (Error.errorBundlePretty err)

action :: Parser Rumor.Node
action = do
  actionName <- hlexeme identifier
  _ <- lexeme (char '(')

  result <-
        Mega.try (lexeme (action4 actionName))
    <|> Mega.try (lexeme (action3 actionName))
    <|> Mega.try (lexeme (action2 actionName))
    <|> Mega.try (lexeme (action1 actionName))
    <|> pure (Rumor.Action0 actionName)

  _ <- char ')' <?> "end parentheses"
  pure result

action1 :: NonEmptyText -> Parser Rumor.Node
action1 actionName = do
  param1 <- stringExpression
  pure (Rumor.Action1 actionName param1)

action2 :: NonEmptyText -> Parser Rumor.Node
action2 actionName = do
  param1 <- lexeme stringExpression
  _ <- lexeme (char ',')
  param2 <- stringExpression
  pure (Rumor.Action2 actionName param1 param2)

action3 :: NonEmptyText -> Parser Rumor.Node
action3 actionName = do
  param1 <- lexeme stringExpression
  _ <- lexeme (char ',')
  param2 <- lexeme stringExpression
  _ <- lexeme (char ',')
  param3 <- stringExpression
  pure (Rumor.Action3 actionName param1 param2 param3)

action4 :: NonEmptyText -> Parser Rumor.Node
action4 actionName = do
  param1 <- lexeme stringExpression
  _ <- lexeme (char ',')
  param2 <- lexeme stringExpression
  _ <- lexeme (char ',')
  param3 <- lexeme stringExpression
  _ <- lexeme (char ',')
  param4 <- stringExpression
  pure (Rumor.Action4 actionName param1 param2 param3 param4)

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

char :: Char -> Parser Char
char = Char.char
