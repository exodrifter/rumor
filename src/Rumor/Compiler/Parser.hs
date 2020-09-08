module Rumor.Compiler.Parser
( ParseError
, parse
, script
) where

import Rumor.Compiler.NodeParser
import Rumor.Object (Node(..), Script)
import Rumor.Parser
import qualified Rumor.Object.Script as Script

import qualified Data.Maybe as Maybe
import qualified Data.Text as T

parse ::
  (HasResolution r) => SourceName -> T.Text -> Either ParseError (Script r)
parse = runParser script

script :: HasResolution r => Parser r (Script r)
script = do
  nodes <- block
  sections <- getSections
  restOfFile

  pure $ Script.init sections nodes

block :: HasResolution r => Parser r [Node r]
block = do
  spaces
  nodes <- withPos . many $ do
    checkIndent
    n <- section *> pure Nothing <|>
         Just <$> node
    spaces
    pure n

  pure $ Maybe.catMaybes nodes

section :: HasResolution r => Parser r ()
section = withPos $ do
  _ <- string "label"
  spaces1
  i <- identifierLabel
  restOfLine

  spaces
  indented
  ns <- block

  addSection i ns
