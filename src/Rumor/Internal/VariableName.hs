module Rumor.Internal.VariableName
( VariableName(..)
, variableNameToText
) where

import Data.Text (Text)
import Data.NonEmptyText (NonEmptyText)
import Rumor.Internal.Unicode (Unicode, unicodeToNET)

import qualified Data.NonEmptyText as NET

-- | The name of a variable.
newtype VariableName = VariableName Unicode
  deriving (Eq, Ord, Show)

variableNameToNET :: VariableName -> NonEmptyText
variableNameToNET (VariableName unicode) = unicodeToNET unicode

variableNameToText :: VariableName -> Text
variableNameToText = NET.toText . variableNameToNET
