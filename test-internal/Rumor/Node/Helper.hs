module Rumor.Node.Helper
( runNodesParser
) where

import Rumor.Node.Parser (script)
import Rumor.Parser (ParseError, runParser)
import Rumor.Script (Script)

import Data.Fixed (E12)
import qualified Data.Text as T

runNodesParser ::
  T.Text ->
  Either ParseError (Script E12)
runNodesParser = runParser script ""
