module Rumor.Compiler.Helper
( parse
, runParser
) where

import Rumor.Expression (Expression(..))
import Rumor.Parser (Parser, ParseError)
import Rumor.Script (Script)
import qualified Rumor.Compiler as Compiler
import qualified Rumor.Parser as Parser

import Data.Fixed (E12)
import qualified Data.Text as T

parse :: T.Text -> Either ParseError (Script E12)
parse = Compiler.parse ""

runParser ::
  Parser (Expression E12 a) ->
  T.Text ->
  Either ParseError (Expression E12 a)
runParser parser = Parser.runParser parser ""
