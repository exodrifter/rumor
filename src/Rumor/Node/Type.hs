module Rumor.Node.Type
( Node(..)
, Identifier(..)
) where

import Rumor.Expression.Type

import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T

data Node r =
  -- Adds dialog to the scene
    Append (Maybe Identifier) (Expression r T.Text)
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
