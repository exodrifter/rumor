module Rumor.Value.Type
( Value(..)
) where

import qualified Data.Text as T

data Value r =
  BooleanValue Bool |
  NumberValue (Fixed r) |
  TextValue T.Text
  deriving stock (Eq, Show)
