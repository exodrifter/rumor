module Rumor.ClearFlag
( ClearFlag(..)
) where

data ClearFlag =
    All
  | Dialog
  | Choices
  deriving stock (Eq, Show)
