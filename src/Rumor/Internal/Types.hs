module Rumor.Internal.Types
  ( Speaker(..)
  , Node(..)
  ) where

import Data.Text (Text)

-- | The identifier for a character who is saying something.
newtype Speaker = Speaker Text
  deriving (Eq, Show)

-- | The nodes represent the abstract syntax tree of a Rumor dialog.
data Node =
    Say (Maybe Speaker) Text
  | Add (Maybe Speaker) Text
  deriving (Eq, Show)
