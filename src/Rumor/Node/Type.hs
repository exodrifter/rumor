module Rumor.Node.Type
( Node(..)
, Identifier(..)
, ClearType(..)
) where

import Rumor.Expression.Type

import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T

data Node r =
  -- Adds dialog to the scene
    Append (Maybe Identifier) (Expression r T.Text)
  -- Waits until the user chooses exactly one choice and removes all choices
  -- from the scene after a choice has been selected
  | Choose
  -- Removes all dialog and choices from the scene
  | Clear ClearType
  -- Temporarily moves execution to a specified section
  | Jump Identifier
  -- Pauses execution for a specified number of milliseconds
  | Pause (Expression r (Fixed r))
  -- Moves execution from the current block to the parent block
  | Return
  -- Sets the dialog in the scene
  | Say (Maybe Identifier) (Expression r T.Text)
  -- A labeled section which execution can be jumped to
  | Section Identifier (NE.NonEmpty (Node r))
  -- Wait for a user to provide input
  | Wait
  deriving stock (Eq, Show)

newtype Identifier = Identifier { unIdentifier :: T.Text }
  deriving stock (Eq, Show)
  deriving newtype (IsString)

data ClearType =
    ClearAll
  | ClearDialog
  | ClearChoices
  deriving stock (Eq, Show)
