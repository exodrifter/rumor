module Rumor.Interpreter.Run
( advance
, choose
, init
) where

import Rumor.Interpreter.Context (Context(..))
import Rumor.Interpreter.Type (Interpreter(..))
import Rumor.Object (Identifier, Node(..), Script)
import qualified Rumor.Expression as Expression
import qualified Rumor.Interpreter.Context as Context
import qualified Rumor.Object.Script as Script

import Control.Monad.State (execState, gets, modify')
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map

init :: HasResolution r => Script r -> Context r
init s = execState (unInterpreter initFastForward) (Context.init s)

-- Called to process the initial nodes in a script when the context is
-- initialized.
initFastForward :: HasResolution r => Interpreter r ()
initFastForward = do
  -- First, we fast-foward the context.
  fastForward

  -- Then, we explicity process cases special to the initial load.
  n <- gets Context.nextNode
  case n of

    -- We don't want to wait for the user to provide manual input the
    -- first time these show up; otherwise there won't be any dialog to
    -- display!
    Just (Append _ _) -> advance
    Just (Say _ _) -> advance

    -- We don't automatically process the wait here, because we assume
    -- that the author may have chosen to display something on screen
    -- using a binded function call or want an explicit user input for
    -- some other reason.
    Just Wait -> pure ()

    -- These other cases don't have any special handling related to
    -- initialization that isn't already taken care of by
    -- fast-forwarding.
    Just (Choice _ _) -> pure ()
    Just Choose -> pure ()
    Just (Clear _) -> pure ()
    Just (Jump _) -> pure ()
    Just (Pause _) -> pure ()
    Just Return -> pure ()
    Nothing -> pure ()

-- Called to process nodes that don't require user input.
fastForward :: HasResolution r => Interpreter r ()
fastForward = do
  n <- gets Context.nextNode
  case n of

    Just (Choice k v) -> do
      modify' $ Context.addChoice k (Expression.evaluateText v)
      increment
      fastForward

    Just (Clear f) -> do
      modify' $ Context.clear f
      increment
      fastForward

    Just (Jump id) -> do
      push id
      fastForward

    Just Return -> do
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

    -- These require some condition to happen before we can proceed.
    Just (Append _ _) -> pure ()
    Just (Say _ _) -> pure ()
    Just Choose -> pure ()
    Just (Pause _) -> pure ()
    Just Wait -> pure ()

-- Called when the user provides input.
advance :: HasResolution r => Interpreter r ()
advance = do
  n <- gets Context.nextNode
  case n of

    Just (Append k v) -> do
      modify' $ Context.addDialog k (Expression.evaluateText v)
      increment
      fastForward

    Just (Say k v) -> do
      modify' $ Context.clearDialog
      modify' $ Context.addDialog k (Expression.evaluateText v)
      increment
      fastForward

    Just Wait -> do
      increment
      fastForward

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
    Just Return -> do
      fastForward
      advance
    Nothing -> do
      fastForward
      m <- gets Context.nextNode
      case m of
        Just _ -> advance
        Nothing -> pure ()

    -- The user can't proceed when these are the current node.
    Just Choose -> pure ()
    Just (Pause _) -> pure ()

-- Called when the user wants to choose a choice.
choose :: HasResolution r => Identifier -> Interpreter r ()
choose id = do
  n <- gets Context.nextNode
  case n of

    Just Choose -> do
      choices <- gets Context.currentChoices
      if Map.member id choices
      then do
        push id
        fastForward
      else
        pure ()

    -- Fast-forward before we try choosing a choice.
    Just (Choice _ _) -> do
      fastForward
      choose id
    Just (Clear _) -> do
      fastForward
      choose id
    Just (Jump _) -> do
      fastForward
      choose id
    Just Return -> do
      fastForward
      choose id
    Nothing -> do
      fastForward
      m <- gets Context.nextNode
      case m of
        Just _ -> choose id
        Nothing -> pure ()

    -- The user can't choose a choice when these are the current node.
    Just (Append _ _) -> pure ()
    Just (Say _ _) -> pure ()
    Just Wait -> pure ()
    Just (Pause _) -> pure ()

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
