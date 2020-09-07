module Rumor.Object.ClearFlag
( ClearFlag(..)
) where

data ClearFlag =
    ClearAll
  | ClearDialog
  | ClearChoices
  deriving stock (Eq, Show)
