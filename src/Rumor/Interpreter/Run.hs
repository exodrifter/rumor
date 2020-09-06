module Rumor.Interpreter.Run
( advance
, init
) where

import Rumor.Interpreter.Context (Context(..))
import Rumor.Interpreter.Type (Interpreter(..))
import Rumor.Node (Node(..), Identifier)
import Rumor.Script (Script(..))
import qualified Rumor.Interpreter.Context as Context
import qualified Rumor.Interpreter.StackFrame as StackFrame
import qualified Rumor.Script as Script

import Control.Monad.State (execState, gets, modify')
import qualified Data.List.NonEmpty as NE

init :: HasResolution r => Script r -> Context r
init s =
  execState
    (unInterpreter fastForward)
    (Context s [StackFrame.init (Script.nodes s)])

-- Called to process nodes that don't require user input
fastForward :: HasResolution r => Interpreter r ()
fastForward = do
  n <- gets Context.currentNode
  case n of
    Just (Choice _ _) -> do
      -- TODO: implement
      increment
      fastForward

    Just (Clear _) -> do
      -- TODO: implement
      increment
      fastForward

    Just (Jump id) -> do
      push id
      fastForward

    Just (Return) -> do
      pop
      increment
      fastForward

    Nothing -> do
      pop
      increment
      sf <- gets Context.currentFrame
      case sf of
        Just _ -> fastForward
        Nothing -> pure ()

    -- These require some condition to happen before we can proceed
    Just (Append _ _) -> pure ()
    Just (Say _ _) -> pure ()
    Just (Choose) -> pure ()
    Just (Pause _) -> pure ()
    Just (Wait) -> pure ()

-- Called when the user provides input
advance :: HasResolution r => Interpreter r ()
advance = do
  n <- gets Context.currentNode
  case n of

    -- Process user input
    Just (Append _ _) -> do
      -- TODO: implement
      increment
      fastForward
    Just (Say _ _) -> do
      -- TODO: implement
      increment
      fastForward
    Just (Wait) -> do
      increment
      fastForward

    -- The user can't proceed when these are the current node.
    Just (Choose) ->
      pure ()
    Just (Pause _) ->
      pure ()

    -- Fast-forward before we try advancing.
    Just (Choice _ _) -> do
      fastForward
      advance
    Just (Clear _) -> do
      fastForward
      advance
    Just (Jump _) -> do
      fastForward
      advance
    Just (Return) -> do
      fastForward
      advance
    Nothing -> do
      fastForward
      m <- gets Context.currentNode
      case m of
        Just _ -> advance
        Nothing -> pure ()

-- Moves the pointer to the next node.
increment :: Interpreter r ()
increment = modify' Context.increment

-- Pushes a new stack frame onto the stack created from a labeled section in the
-- script. If the labeled section does not exist, then nothing happens.
push :: Identifier -> Interpreter r ()
push id = do
  s <- gets Context.script
  case NE.toList <$> Script.lookup id s of
    Just ns -> modify' $ Context.push ns
    Nothing -> pure ()

-- Pops the current stack frame from the stack. If there is no stack frame to
-- pop, then nothing happens.
pop :: Interpreter r ()
pop =
  modify' Context.pop
