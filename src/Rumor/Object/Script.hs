module Rumor.Object.Script
( Script(..)
, empty
, init
, lookup
) where

import Rumor.Object.Identifier (Identifier)
import Rumor.Object.Node (Node)

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

init :: Map Identifier (NE.NonEmpty (Node r)) -> [Node r] -> Script r
init = Script

lookup :: Identifier -> Script r -> Maybe (NE.NonEmpty (Node r))
lookup k s = Map.lookup k (sections s)
