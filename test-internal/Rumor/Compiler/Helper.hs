module Rumor.Compiler.Helper
( runParser
) where

import Rumor.Expression (Expression(..))
import Rumor.Parser (Parser, ParseError)
import qualified Rumor.Parser as Parser

import Data.Fixed (E12)
import qualified Data.Text as T

runParser ::
  Parser (Expression E12 a) ->
  T.Text ->
  Either ParseError (Expression E12 a)
runParser parser = Parser.runParser parser ""
