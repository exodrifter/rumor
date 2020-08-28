module Rumor.Value
( Value(..)
) where

import Rumor.Prelude
import qualified Data.Text as T

data Value r =
  BooleanValue Bool |
  NumberValue (Fixed r) |
  TextValue T.Text
  deriving stock (Eq, Show)
