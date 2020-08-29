module Rumor.Node.Parser
( say
, append
) where

import Rumor.Expression.Parser
import Rumor.Expression.Type
import Rumor.Node.Type
import Rumor.Parser

import qualified Data.Text as T

identifier :: Parser Identifier
identifier = Identifier . T.pack <$> many1 alphaNum

say :: HasResolution r => Parser (Node r)
say = do
  (s, d) <- dialog ':'
  pure $ Say s d

append :: HasResolution r => Parser (Node r)
append = do
  (s, d) <- dialog '+'
  pure $ Append s d

dialog ::
  HasResolution r =>
  Char ->
  Parser (Maybe Identifier, Expression r T.Text)
dialog symbol = do
  s <- Just <$> identifier <|> pure Nothing
  spaces
  _ <- char symbol
  d <- withPos text
  pure $ (s, d)
