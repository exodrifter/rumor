module Rumor.Compiler.Parser
( ParseError
, parse
, script
) where

import Rumor.Compiler.NodeParser
import Rumor.Object (Script)
import Rumor.Parser
import qualified Rumor.Object.Script as Script

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
