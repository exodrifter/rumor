module Rumor.Internal.Node
  ( Node(..)
  , Label(..)
  , Speaker(..)
  , ClearType(..)
  ) where

import Data.List.NonEmpty (NonEmpty)
import Rumor.Internal.Expression (Expression(..))
import Rumor.Internal.Unicode (Unicode(..))
import Rumor.Internal.VariableName (VariableName(..))

-- | The nodes represent the abstract syntax tree of a Rumor dialog.
data Node =
    Say (Maybe Speaker) Expression (Maybe Label)
  | Add (Maybe Speaker) Expression (Maybe Label)
  | Control Expression (NonEmpty Node) (Maybe (NonEmpty Node))
  | Action0 VariableName
  | Action1 VariableName Expression
  | Action2 VariableName Expression Expression
  | Action3 VariableName Expression Expression Expression
  | Action4 VariableName Expression Expression Expression Expression
  | Choice Expression (Maybe Label) (Maybe (NonEmpty Node))
  | Clear ClearType
  deriving (Eq, Show)

-- | The identifier for a node.
newtype Label = Label Unicode
  deriving (Eq, Show)

-- | The identifier for a character who is saying something.
newtype Speaker = Speaker Unicode
  deriving (Eq, Show)

data ClearType =
    ClearAll
  | ClearChoice Label
  | ClearChoices
  | ClearDialog
  deriving (Eq, Show)
