module Rumor.Object.Identifier
( Identifier(..)
) where

import qualified Data.Text as T

newtype Identifier = Identifier { unIdentifier :: T.Text }
  deriving stock (Eq, Ord, Show)
  deriving newtype (IsString)
