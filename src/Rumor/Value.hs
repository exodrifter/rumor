{-# LANGUAGE Safe #-}
{-# LANGUAGE DerivingStrategies #-}

module Rumor.Value
( Value(..)
) where

import Rumor.Prelude
import qualified Data.Text as T

data Value =
  BooleanValue Bool |
  NumberValue Double |
  TextValue T.Text
  deriving stock (Eq, Show)
