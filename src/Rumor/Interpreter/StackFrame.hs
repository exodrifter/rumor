module Rumor.Interpreter.StackFrame
( StackFrame
, init

, current
, increment
) where

import Rumor.Object.Node (Node)

import qualified Safe

data StackFrame r =
  StackFrame
    { nodes :: [Node r]
    , pointer :: Int
    }
  deriving stock (Eq, Show)

init :: [Node r] -> StackFrame r
init ns = StackFrame ns 0

current :: StackFrame r -> Maybe (Node r)
current sf = Safe.atMay (nodes sf) (pointer sf)

increment :: StackFrame r -> StackFrame r
increment sf = sf { pointer = (pointer sf) + 1 }
