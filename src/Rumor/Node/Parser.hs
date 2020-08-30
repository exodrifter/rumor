module Rumor.Node.Parser
( nodes
, say
, append
, section
) where

import Rumor.Expression
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
  pause <|>
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

pause :: HasResolution r => Parser (Node r)
pause = do
  let millisecondsParser = do
        n <- math
        _ <- (spaces1 *> string "milliseconds") <|> string "ms"
        pure n
      secondsParser = do
        n <- math
        _ <- (spaces1 *> string "seconds") <|> string "s"
        pure $ simplifyMath (Multiply n (Number 1000))
      minutesParser = do
        n <- math
        _ <- (spaces1 *> string "minutes") <|> string "m"
        pure $ simplifyMath (Multiply n (Number 60000))
  _ <- string "pause"
  spaces1
  t <- millisecondsParser <|>
       secondsParser <|>
       minutesParser
  _ <- restOfLine
  pure $ Pause t

say :: HasResolution r => Parser (Node r)
say = do
  (s, d) <- dialog ':'
  pure $ Say s d

section :: HasResolution r => Parser (Node r)
section = withPos $ do
  _ <- string "label"
  spaces1
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
