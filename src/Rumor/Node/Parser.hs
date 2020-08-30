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

import Data.Int (Int)
import GHC.Enum (maxBound)
import qualified Data.List.NonEmpty as NE
import qualified Data.Set as Set
import qualified Data.Text as T

nodes :: HasResolution r => Parser [Node r]
nodes = do
  spaces
  ns <- withPos . many $ do
    checkIndent
    n <- node
    spaces
    pure n
  restOfFile
  pure ns

node :: HasResolution r => Parser (Node r)
node =
  append <|>
  choice <|>
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

choice :: HasResolution r => Parser (Node r)
choice = do
  _ <- string "choice"
  i <- option $ spaces1 *> identifierLabel
  spaces1
  c <- text
  _ <- restOfLine
  pure $ Choice i c

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
  i <- identifierLabel
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
  Parser (Maybe Character, Expression r T.Text)
dialog symbol = do
  i <- option character
  spaces
  _ <- char symbol
  d <- text
  pure $ (i, d)

character :: Parser Character
character = Character . T.pack <$> many1 alphaNum

identifier :: Parser Identifier
identifier = Identifier . T.pack <$> many1 alphaNum

identifierLabel :: Parser Identifier
identifierLabel = do
  _ <- char '['
  spaces

  i <- identifier
  usedIdentifiers <- getState
  if Set.member i usedIdentifiers
  then fail "duplicate identifier label"
  else
    if Set.size usedIdentifiers >= (maxBound :: Int)
    then fail "cannot create any more identifier labels"
    else modifyState (Set.insert i)

  spaces
  _ <- char ']'
  pure i
