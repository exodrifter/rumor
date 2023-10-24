module Rumor.Internal.Node
  ( Node(..)
  , Label(..)
  , Speaker(..)
  ) where

import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)
import Rumor.Internal.Expression (Expression(..))
import Rumor.Internal.Unicode (Unicode(..))
import Rumor.Internal.VariableName (VariableName(..))

-- | The nodes represent the abstract syntax tree of a Rumor dialog.
data Node =
    Say (Maybe Speaker) (Expression Text) (Maybe Label)
  | Add (Maybe Speaker) (Expression Text) (Maybe Label)
  | Control (Expression Bool) (NonEmpty Node) (Maybe (NonEmpty Node))
  | Action0 VariableName
  | Action1 VariableName (Expression Text)
  | Action2 VariableName (Expression Text) (Expression Text)
  | Action3 VariableName (Expression Text) (Expression Text) (Expression Text)
  | Action4 VariableName (Expression Text) (Expression Text) (Expression Text) (Expression Text)
  | Choice (Expression Text) (Maybe Label) (Maybe (NonEmpty Node))
  deriving (Eq, Show)

-- | The identifier for a node.
newtype Label = Label Unicode
  deriving (Eq, Show)

-- | The identifier for a character who is saying something.
newtype Speaker = Speaker Unicode
  deriving (Eq, Show)
