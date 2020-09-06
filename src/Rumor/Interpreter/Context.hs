module Rumor.Interpreter.Context
( Context(..)
, currentNode
, currentFrame
, increment
, push
, pop
) where

import Rumor.Script (Script)
import Rumor.Node.Type (Node)
import Rumor.Interpreter.StackFrame (StackFrame)
import qualified Rumor.Interpreter.StackFrame as StackFrame

data Context r =
  Context
    { script :: Script r
    , stack :: [StackFrame r]
    }
  deriving stock (Eq, Show)

-- Gets the current node the context is pointing to.
currentNode :: Context r -> Maybe (Node r)
currentNode c = do
  sf <- currentFrame c
  StackFrame.current sf

-- Gets the current stack frame the context is pointing to.
currentFrame :: Context r -> Maybe (StackFrame r)
currentFrame c =
  case stack c of
    x:_ -> Just x
    [] -> Nothing

-- Increments the pointer in the current stack
increment :: Context r -> Context r
increment c =
  case stack c of
    x:xs -> c { stack = StackFrame.increment x : xs}
    [] -> c

-- Pushes a new stack frame onto the stack.
push :: [Node r] -> Context r -> Context r
push ns c = c { stack = StackFrame.init ns : stack c }

-- Pops the current stack frame from the stack. If the stack is empty, nothing
-- happens.
pop :: Context r -> Context r
pop c =
  case stack c of
    _:xs -> c { stack = xs }
    [] -> c
