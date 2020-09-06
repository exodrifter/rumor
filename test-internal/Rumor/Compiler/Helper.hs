module Rumor.Compiler.Helper
( runNodesParser
, runTestParser
) where

import Rumor.Expression (Expression(..))
import Rumor.Parser (Parser, ParseError)
import Rumor.Script (Script)
import qualified Rumor.Compiler as Compiler
import qualified Rumor.Parser as Parser

import Data.Fixed (E12)
import qualified Data.Text as T

runNodesParser :: T.Text -> Either ParseError (Script E12)
runNodesParser = Compiler.parse ""

runTestParser ::
  Parser (Expression E12 a) ->
  T.Text ->
  Either ParseError (Expression E12 a)
runTestParser parser = Parser.runParser parser ""
