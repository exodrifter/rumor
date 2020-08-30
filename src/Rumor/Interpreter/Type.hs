{-# LANGUAGE MultiParamTypeClasses #-}

module Rumor.Interpreter.Type
( Interpreter
) where

import Rumor.Interpreter.Context

import Control.Applicative (Applicative(..))
import Control.Monad (Monad(..))
import Control.Monad.State (State, MonadState(..))
import Data.Functor (Functor(..))

newtype Interpreter r a =
  Interpreter
    { unInterpreter :: State (Context r) a
    }

instance Functor (Interpreter r) where
  fmap fn i = Interpreter $ fmap fn (unInterpreter i)

instance Applicative (Interpreter r) where
  pure a = Interpreter $ pure a
  l <*> r = Interpreter $ unInterpreter l <*> unInterpreter r

instance Monad (Interpreter r) where
  return = pure
  p >>= f = Interpreter $ unInterpreter p >>= (unInterpreter . f)

instance MonadState (Context r) (Interpreter r) where
  state = Interpreter . state
