module Rumor.Script
( Script(..)
, empty
, singleton
) where

import Rumor.Node.Type

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

singleNode :: Node r -> Script r
singleNode n = Script Map.empty [n]
