module Rumor.Compiler.NodeParser
( node
, identifierLabel
) where

import Prelude hiding (return)
import Rumor.Compiler.ExpressionParser
import Rumor.Expression
import Rumor.Node
import Rumor.Parser
import qualified Rumor.ClearFlag as ClearFlag

import GHC.Enum (maxBound)
import qualified Data.Set as Set
import qualified Data.Text as T

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
  restOfLine
  pure $ Choice i c

choose :: Parser (Node r)
choose = do
  _ <- string "choose"
  restOfLine
  pure Choose

clear :: Parser (Node r)
clear = do
  _ <- string "clear"
  spaces1
  t <- string "dialog" *> pure ClearFlag.Dialog <|>
       string "choices" *> pure ClearFlag.Choices <|>
       string "all" *> pure ClearFlag.All
  restOfLine
  pure $ Clear t

jump :: Parser (Node r)
jump = do
  _ <- string "jump"
  spaces1
  i <- identifier
  restOfLine
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
  restOfLine
  pure $ Pause t

return :: Parser (Node r)
return = do
  _ <- string "return"
  restOfLine
  pure Return

say :: HasResolution r => Parser (Node r)
say = do
  (s, d) <- dialog ':'
  pure $ Say s d

wait :: Parser (Node r)
wait = do
  _ <- string "wait"
  restOfLine
  pure Wait

--------------------------------------------------------------------------------
-- Helper Parsers
--------------------------------------------------------------------------------

dialog ::
  HasResolution r =>
  Char ->
  Parser (Maybe Character, Expression r T.Text)
dialog symbol = withPos $ do
  i <- option character
  spaces
  same
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
