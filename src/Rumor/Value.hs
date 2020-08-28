{-# LANGUAGE Safe #-}

module Rumor.Value
( Value(..)
) where

import Rumor.Prelude
import qualified Data.Text as T

data Value =
  BooleanValue Bool |
  NumberValue Pico |
  TextValue T.Text
  deriving stock (Eq, Show)
