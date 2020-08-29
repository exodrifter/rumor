module Rumor.Node.Helper
( runNodeParser
, runNodesParser
) where

import Rumor.Node.Type (Node(..))
import Rumor.Parser

import Data.Fixed (E12)
import qualified Data.Text as T

runNodeParser ::
  Parser (Node E12) ->
  T.Text ->
  Either ParseError (Node E12)
runNodeParser parser = runParser parser ""

runNodesParser ::
  Parser [Node E12] ->
  T.Text ->
  Either ParseError [Node E12]
runNodesParser parser = runParser parser ""
