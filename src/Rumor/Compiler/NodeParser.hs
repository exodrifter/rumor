module Rumor.Compiler.NodeParser
( node
, identifierLabel
) where

import Prelude hiding (return)
import Rumor.Compiler.ExpressionParser
import Rumor.Expression
import Rumor.Object
import Rumor.Parser

import GHC.Enum (maxBound)
import qualified Crypto.Hash as Hash
import qualified Data.ByteArray.Encoding as BA
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

node :: HasResolution r => Parser r (Node r)
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

append :: HasResolution r => Parser r (Node r)
append = do
  (s, d) <- dialog '+'
  pure $ Append s d

choice :: HasResolution r => Parser r (Node r)
choice = do
  _ <- string "choice"
  spaces1
  i <- identifierLabel <* spaces1 <|>
       generatedIdentifierLabel
  c <- text
  pure $ Choice i c

choose :: Parser r (Node r)
choose = do
  _ <- string "choose"
  restOfLine
  pure Choose

clear :: Parser r (Node r)
clear = do
  _ <- string "clear"
  spaces1
  t <- string "dialog" *> pure ClearDialog <|>
       string "choices" *> pure ClearChoices <|>
       string "all" *> pure ClearAll
  restOfLine
  pure $ Clear t

jump :: Parser r (Node r)
jump = do
  _ <- string "jump"
  spaces1
  i <- identifier
  restOfLine
  pure $ Jump i

pause :: HasResolution r => Parser r (Node r)
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

return :: Parser r (Node r)
return = do
  _ <- string "return"
  restOfLine
  pure Return

say :: HasResolution r => Parser r (Node r)
say = do
  (s, d) <- dialog ':'
  pure $ Say s d

wait :: Parser r (Node r)
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
  Parser r (Maybe Character, Expression r T.Text)
dialog symbol = withPos $ do
  i <- option character
  spaces
  same
  _ <- char symbol
  d <- text
  pure $ (i, d)

-- A label is the base parser for a character or identifier. It can
-- contain any alphabetic or numeric unicode characters, but it
-- *cannot* start with an underscore as these are reserved for use by
-- Rumor (like when automatically generating labels for choices).
label :: Parser r T.Text
label = do
  cs <- many1 alphaNum
  case cs of
    '_':_ -> fail "labels cannot start with '_'"
    _ -> pure $ T.pack cs

character :: Parser r Character
character = Character <$> label

identifier :: Parser r Identifier
identifier = Identifier <$> label

identifierLabel :: Parser r Identifier
identifierLabel = do
  _ <- char '['
  spaces

  i <- identifier
  markIdentifierAsUsed i

  spaces
  _ <- char ']'
  pure i

-- Should be used when an identifier is required, but the user is not
-- required to supply one. Note the check on the first line which
-- ensures that it's not being used when a user *tried* to define their
-- own identifier, because we would still want the original failure in
-- that case.
generatedIdentifierLabel :: Parser r Identifier
generatedIdentifierLabel = do
  notFollowedBy $ char '['
  str <- getRandoms 120
  let
    i = Identifier
      . ("_" <>)
      . TE.decodeUtf8
      . BA.convertToBase BA.Base64
      . Hash.hashWith Hash.SHA1
      . BS8.pack
      $ str
  markIdentifierAsUsed i
  pure i

markIdentifierAsUsed :: Identifier -> Parser r ()
markIdentifierAsUsed i = do
  usedIdentifiers <- getUsedIdentifiers
  if Set.member i usedIdentifiers
  then fail "duplicate identifier label"
  else
    if Set.size usedIdentifiers >= (maxBound :: Int)
    then fail "cannot create any more identifier labels"
    else setUsedIdentifiers (Set.insert i usedIdentifiers)
