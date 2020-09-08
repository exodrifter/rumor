module Rumor.Interpreter.Context
( Context
, init

-- Getters
, script
, currentChoices
, currentDialog
, currentDialogFor
, currentFrame
, nextNode

-- Mutators
, addChoice
, addDialog
, clear
, clearAll
, clearChoices
, clearDialog
, increment
, pop
, push
) where

import Rumor.Interpreter.StackFrame (StackFrame)
import Rumor.Object (Character, ClearFlag(..), Identifier, Node, Script)
import qualified Rumor.Interpreter.StackFrame as StackFrame
import qualified Rumor.Object.Script as Script

import qualified Data.Text as T
import qualified Data.Map.Strict as Map

data Context r =
  Context
    { script :: Script r
    , stack :: [StackFrame r]
    , dialog :: Map (Maybe Character) T.Text
    , choices :: Map Identifier T.Text
    }
  deriving stock (Eq, Show)

init :: Script r -> Context r
init s =
  Context
    { script = s
    , stack = [StackFrame.init (Script.nodes s)]
    , dialog = Map.empty
    , choices = Map.empty
    }

--------------------------------------------------------------------------------
-- Getters
--------------------------------------------------------------------------------

-- Gets the current choices that can currently be seen by the player.
currentChoices :: Context r -> Map Identifier T.Text
currentChoices = choices

-- Gets the current dialog that can currently be seen by the player.
currentDialog :: Context r -> Map (Maybe Character) T.Text
currentDialog = dialog

-- Gets the current dialog that can currently be seen by the player.
currentDialogFor :: Maybe Character -> Context r -> Maybe T.Text
currentDialogFor k = Map.lookup k . dialog

-- Gets the current stack frame the context is pointing to.
currentFrame :: Context r -> Maybe (StackFrame r)
currentFrame c =
  case stack c of
    x:_ -> Just x
    [] -> Nothing

-- Gets the next node in the context that should be processed
nextNode :: Context r -> Maybe (Node r)
nextNode c = do
  sf <- currentFrame c
  StackFrame.current sf

--------------------------------------------------------------------------------
-- Mutators
--------------------------------------------------------------------------------

addChoice :: Identifier -> T.Text -> Context r -> Context r
addChoice k v c = c { choices = Map.insert k v (choices c) }

addDialog :: Maybe Character -> T.Text -> Context r -> Context r
addDialog k v c =
  let updateDialog mt =
        case mt of
          Nothing -> Just v
          Just t -> Just $ t <> " " <> v
  in  c { dialog = Map.alter updateDialog k (dialog c) }

clear :: ClearFlag -> Context r -> Context r
clear f =
  case f of
    ClearChoices -> clearChoices
    ClearDialog -> clearDialog
    ClearAll -> clearAll

clearAll :: Context r -> Context r
clearAll =
    clearChoices
  . clearDialog

clearChoices :: Context r -> Context r
clearChoices c = c { choices = Map.empty }

clearDialog :: Context r -> Context r
clearDialog c = c { dialog = Map.empty }

-- Increments the pointer in the current stack
increment :: Context r -> Context r
increment c =
  case stack c of
    x:xs -> c { stack = StackFrame.increment x : xs}
    [] -> c

-- Pops the current stack frame from the stack. If the stack is empty, nothing
-- happens.
pop :: Context r -> Context r
pop c =
  case stack c of
    _:xs -> c { stack = xs }
    [] -> c

-- Pushes a new stack frame onto the stack.
push :: [Node r] -> Context r -> Context r
push ns c = c { stack = StackFrame.init ns : stack c }
