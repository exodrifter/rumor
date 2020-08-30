module Rumor.Interpreter.Context
( Context(..)
, incrementPointer
) where

import Rumor.Node

data Context r = Context
  { stackframe :: [Node r]
  , pointer :: Integer
  }

incrementPointer :: Context r -> Context r
incrementPointer c = c { pointer = (pointer c) + 1 }
