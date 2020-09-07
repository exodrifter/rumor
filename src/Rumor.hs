module Rumor
( Rumor.Expression.Expression(..)
, Rumor.Expression.evaluate
, Rumor.Expression.simplify

, Rumor.Object.ClearFlag.ClearFlag(..)
, Rumor.Object.Node(..)
, Rumor.Object.Script.Script(..)
, Rumor.Object.Value(..)

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
import qualified Rumor.Object
import qualified Rumor.Object.ClearFlag
import qualified Rumor.Object.Script

import Control.Monad.State (execState)

advance :: (HasResolution r) => Context r -> Context r
advance = execState . unInterpreter $ Rumor.Interpreter.advance
