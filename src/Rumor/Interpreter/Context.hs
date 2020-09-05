module Rumor.Interpreter.Context
( Context
, init

-- Operations
, next
, push
, pop
) where

import Rumor.Script (Script)
import Rumor.Node.Type (Node, Identifier)
import Rumor.Interpreter.StackFrame (StackFrame)
import qualified Rumor.Script as Script
import qualified Rumor.Interpreter.StackFrame as StackFrame

import qualified Data.List.NonEmpty as NE

data Context r =
  Context
    { script :: Script r
    , stack :: [StackFrame r]
    }
  deriving stock (Eq, Show)

init :: Script r -> Context r
init s = Context s [StackFrame.init (Script.nodes s)]

--------------------------------------------------------------------------------
-- Operations
--------------------------------------------------------------------------------

-- Returns the next node to execute. If the stack is empty, then no node will be
-- returned
next :: Context r -> (Maybe (Node r), Context r)
next c =
  case stack c of

    -- Stack has at least one stack frame
    x:xs ->
      let n = StackFrame.current x
          sf = StackFrame.increment x
      in  case StackFrame.current sf of
            Nothing ->
              -- Pop the exhausted stack frame from the stack
              (n, c { stack = xs })
            Just _ ->
              -- The stack frame is not exhausted
              (n, c { stack = sf:xs })

    -- Stack is empty
    [] -> (Nothing, c)

-- Pushes a new stack frame onto the stack created from a labeled section. If
-- the labeled section does not exist, then nothing happens.
push :: Identifier -> Context r -> ((), Context r)
push id c =
  case NE.toList <$> Script.lookup id (script c) of
    Just ns -> ((), c { stack = StackFrame.init ns : stack c })
    Nothing -> ((), c)

-- Pops the current stack frame from the stack. If there is no stack frame to
-- pop, then nothing happens.
pop :: Context r -> ((), Context r)
pop c =
  case stack c of
    _:xs -> ((), c { stack = xs })
    [] -> ((), c)
