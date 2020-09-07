module Rumor.Compiler.Parser
( ParseError
, parse
, script
) where

import Rumor.Compiler.NodeParser
import Rumor.Object (Identifier, Node(..), Script)
import Rumor.Parser
import qualified Rumor.OneOf as OO
import qualified Rumor.Object.Script as Script

import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import qualified Data.Text as T

parse ::
  (HasResolution r) => SourceName -> T.Text -> Either ParseError (Script r)
parse = runParser script

script :: HasResolution r => Parser (Script r)
script = block <* restOfFile

block :: HasResolution r => Parser (Script r)
block = do
  spaces
  sectionsAndNodes <- withPos . many $ do
    checkIndent
    n <- OO.First <$> section <|>
         OO.Second <$> node
    spaces
    pure n

  pure $ Script.init
    (Map.unions $ OO.firsts sectionsAndNodes)
    (OO.seconds sectionsAndNodes)

section :: HasResolution r => Parser (Map Identifier (NE.NonEmpty (Node r)))
section = withPos $ do
  _ <- string "label"
  spaces1
  i <- identifierLabel
  restOfLine

  spaces
  indented
  s <- block
  case NE.nonEmpty (Script.nodes s) of
    Just ns -> pure $ Map.insert i ns (Script.sections s)
    Nothing -> fail "labeled section does not contain anything"
