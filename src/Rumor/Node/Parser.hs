module Rumor.Node.Parser
( nodes
, say
, append
, section
) where

import Rumor.Expression.Parser
import Rumor.Expression.Type
import Rumor.Node.Type
import Rumor.Parser

import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T

identifier :: Parser Identifier
identifier = Identifier . T.pack <$> many1 alphaNum

nodes :: HasResolution r => Parser [Node r]
nodes = many node

node :: HasResolution r => Parser (Node r)
node =
  append <|>
  say <|>
  section <|>
  wait

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
  d <- text
  pure $ (s, d)

say :: HasResolution r => Parser (Node r)
say = do
  (s, d) <- dialog ':'
  pure $ Say s d

section :: HasResolution r => Parser (Node r)
section = withPos $ do
  _ <- string "label"
  spaces
  i <- identifier
  _ <- restOfLine

  spaces
  n <- indented *> block (node <* spaces)
  case NE.nonEmpty n of
    Just ns -> pure $ Section i ns
    Nothing -> fail "labeled section does not contain anything"

wait :: Parser (Node r)
wait = do
  _ <- string "wait"
  _ <- restOfLine
  pure Wait
