module Rumor.Parser.Surround
( parenthesis
, braces
) where

import Rumor.Parser.Common (Parser)
import Text.Megaparsec ((<?>))

import qualified Rumor.Parser.Lexeme as Lexeme
import qualified Text.Megaparsec.Char as Char

parenthesis :: Parser a -> Parser a
parenthesis =
  surround
    (Lexeme.lexeme (Char.char '('))
    (Lexeme.lexeme (Char.char ')') <?> "end parenthesis")

braces :: Parser a -> Parser a
braces =
  surround
    (Lexeme.lexeme (Char.char '{'))
    (Lexeme.lexeme (Char.char '}') <?> "end braces")

surround :: Parser a -> Parser b -> Parser c -> Parser c
surround begin end inner = do
  _ <- begin
  result <- inner
  _ <- end
  pure result
