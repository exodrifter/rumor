module Rumor.Node.Parser
( nodes
, say
, append
, section
) where

import Prelude hiding (return)
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
  choose <|>
  clear <|>
  jump <|>
  pause <|>
  return <|>
  say <|>
  section <|>
  wait

--------------------------------------------------------------------------------
-- Node Parsers
--------------------------------------------------------------------------------
-- It is expected that each of these parsers:
-- * Assume that they start parsing where the command for the node is
-- * Consume all of the input on the line, including the newline characters

append :: HasResolution r => Parser (Node r)
append = do
  (s, d) <- dialog '+'
  pure $ Append s d

choose :: Parser (Node r)
choose = do
  _ <- string "choose"
  _ <- restOfLine
  pure Choose

clear :: Parser (Node r)
clear = do
  _ <- string "clear"
  t <- spaces1 *> string "dialog" *> pure ClearDialog <|>
       spaces1 *> string "choices" *> pure ClearChoices <|>
       pure ClearAll
  _ <- restOfLine
  pure $ Clear t

jump :: Parser (Node r)
jump = do
  _ <- string "jump"
  spaces1
  i <- identifier
  _ <- restOfLine
  pure $ Jump i

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

return :: Parser (Node r)
return = do
  _ <- string "return"
  _ <- restOfLine
  pure Return

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

--------------------------------------------------------------------------------
-- Helper Parsers
--------------------------------------------------------------------------------

dialog ::
  HasResolution r =>
  Char ->
  Parser (Maybe Identifier, Expression r T.Text)
dialog symbol = do
  i <- Just <$> identifier <|> pure Nothing
  spaces
  _ <- char symbol
  d <- text
  pure $ (i, d)
