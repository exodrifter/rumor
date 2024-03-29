module Rumor.Object.Node
( Node(..)
) where

import Rumor.Expression (Expression)
import Rumor.Object.Character (Character)
import Rumor.Object.ClearFlag (ClearFlag)
import Rumor.Object.Identifier (Identifier)

import qualified Data.Text as T

data Node r =
  -- Adds dialog to the scene
    Append (Maybe Character) (Expression r T.Text)
  -- Add a choice to the scene a user can pick
  | Choice Identifier (Expression r T.Text)
  -- Waits until the user chooses exactly one choice and removes all choices
  -- from the scene after a choice has been selected
  | Choose
  -- Removes all dialog and choices from the scene
  | Clear ClearFlag
  -- Temporarily moves execution to a specified section
  | Jump Identifier
  -- Pauses execution for a specified number of milliseconds
  | Pause (Expression r (Fixed r))
  -- Moves execution from the current block to the parent block
  | Return
  -- Sets the dialog in the scene
  | Say (Maybe Character) (Expression r T.Text)
  -- Wait for a user to provide input
  | Wait
  deriving stock (Eq, Show)
