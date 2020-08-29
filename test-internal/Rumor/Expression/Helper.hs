module Rumor.Expression.Helper
( runTestParser
) where

import Rumor.Expression.Type (Expression(..))
import Rumor.Parser

import Data.Fixed (E12)
import qualified Data.Text as T

runTestParser ::
  Parser (Expression E12 a) ->
  T.Text ->
  Either ParseError (Expression E12 a)
runTestParser parser = runParser parser ""
