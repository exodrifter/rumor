module Rumor where

import Data.List.NonEmpty (NonEmpty(..))
import Data.NonEmptyText (NonEmptyText)
import Text.Megaparsec ((<?>), (<|>))
import Rumor.Parser
  ( Parser
  , braces
  , hlexeme
  , identifier
  , lexeme
  , space
  , stringExpression
  , booleanExpression
  , say
  , add
  )

import qualified Data.Text as T
import qualified Text.Megaparsec as Mega
import qualified Text.Megaparsec.Char as Char
import qualified Text.Megaparsec.Char.Lexer as Lexer
import qualified Text.Megaparsec.Error as Error
import qualified Rumor.Internal.Types as Rumor

parse :: String -> T.Text -> Either String [Rumor.Node]
parse fileName fileContents =
  case Mega.runParser parser fileName fileContents of
    Right result -> Right result
    Left err -> Left (Error.errorBundlePretty err)

parser :: Parser [Rumor.Node]
parser =
  Lexer.nonIndented space do
    Mega.manyTill (hlexeme node) (Mega.hidden Mega.eof)

node :: Parser Rumor.Node
node =
      Mega.try say
  <|> Mega.try add
  <|> Mega.try action
  <|> control

control :: Parser Rumor.Node
control = do
  currentIndentation <- Lexer.indentGuard space EQ =<< Lexer.indentLevel

  _ <- hlexeme (Char.string "if")
  condition <- braces booleanExpression <|> booleanExpression

  let
    indentedNodes = do
      Lexer.indentBlock space do
        ref <- Lexer.indentGuard space GT currentIndentation
        firstNode <- node
        pure (Lexer.IndentMany (Just ref) (pure . (firstNode :|)) node)
  successBlock <- indentedNodes <?> "indented nodes"

  let elseBlock = do
        _ <- Lexer.indentGuard space EQ currentIndentation
        _ <- hlexeme (Char.string "else")
        indentedNodes <?> "indented nodes"
  failureBlock <- Mega.optional elseBlock

  pure (Rumor.Control condition successBlock failureBlock)

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
