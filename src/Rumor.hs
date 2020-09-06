module Rumor
( Rumor.Expression.Expression(..)
, Rumor.Expression.evaluate
, Rumor.Expression.simplify

, Rumor.Node.Node(..)
, Rumor.Script.Script
, Rumor.Value.Value(..)

, Rumor.Interpreter.Context
, Rumor.Interpreter.currentNode
, Rumor.Interpreter.init

, parse
, advance
) where

import Rumor.Interpreter (Context, unInterpreter)
import Rumor.Node.Parser (script)
import Rumor.Parser (SourceName, ParseError, runParser)
import Rumor.Script (Script)
import qualified Rumor.Expression
import qualified Rumor.Interpreter
import qualified Rumor.Node
import qualified Rumor.Value

import Control.Monad.State (execState)
import qualified Data.Text as T

parse ::
  (HasResolution r) => SourceName -> T.Text -> Either ParseError (Script r)
parse a b = runParser script a b

advance :: (HasResolution r) => Context r -> Context r
advance = execState . unInterpreter $ Rumor.Interpreter.advance
