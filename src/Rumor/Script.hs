module Rumor.Script
( Script(..)
, empty
, singleton

, lookup
) where

import Rumor.Node

import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map

data Script r =
  Script
    { sections :: Map Identifier (NE.NonEmpty (Node r))
    , nodes :: [Node r]
    }
  deriving stock (Eq, Show)

empty :: Script r
empty = Script Map.empty []

singleton :: Node r -> Script r
singleton n = Script Map.empty [n]

lookup :: Identifier -> Script r -> Maybe (NE.NonEmpty (Node r))
lookup k s = Map.lookup k (sections s)
