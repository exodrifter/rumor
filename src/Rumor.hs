module Rumor
( Rumor.Expression.Expression(..)
, Rumor.Expression.evaluate
, Rumor.Expression.simplify

, Rumor.ClearFlag.ClearFlag(..)
, Rumor.Node.Node(..)
, Rumor.Value.Value(..)

, Rumor.Script.Script(..)

, Rumor.Compiler.ParseError
, Rumor.Compiler.parse

, Rumor.Interpreter.Context
, Rumor.Interpreter.addDialog
, Rumor.Interpreter.clear
, Rumor.Interpreter.clearAll
, Rumor.Interpreter.clearChoices
, Rumor.Interpreter.clearDialog
, Rumor.Interpreter.currentChoices
, Rumor.Interpreter.currentDialog
, Rumor.Interpreter.currentDialogFor
, Rumor.Interpreter.init
, Rumor.Interpreter.nextNode

, advance
) where

import Rumor.Interpreter (Context, unInterpreter)
import qualified Rumor.Compiler
import qualified Rumor.Expression
import qualified Rumor.Interpreter
import qualified Rumor.Node
import qualified Rumor.ClearFlag
import qualified Rumor.Script
import qualified Rumor.Value

import Control.Monad.State (execState)

advance :: (HasResolution r) => Context r -> Context r
advance = execState . unInterpreter $ Rumor.Interpreter.advance
