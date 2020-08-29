module Rumor.Node.Helper
( runTestParser
) where

import Rumor.Node.Type (Node(..))
import Rumor.Parser

import Data.Fixed (E12)
import qualified Data.Text as T

runTestParser ::
  Parser (Node E12) ->
  T.Text ->
  Either ParseError (Node E12)
runTestParser parser = runParser parser ""
