module Rumor.Object.Character
( Character(..)
) where

import qualified Data.Text as T

newtype Character = Character { unCharacter :: T.Text }
  deriving stock (Ord, Eq, Show)
  deriving newtype (IsString)
