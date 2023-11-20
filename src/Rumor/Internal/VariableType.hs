module Rumor.Internal.VariableType
( VariableType(..)
, typeToText
) where

import Data.Text (Text)

import qualified Data.Text as T

data VariableType =
    BooleanType
  | NumberType
  | StringType
  | ActionType [VariableType]
  deriving (Eq, Ord, Show)

typeToText :: VariableType -> Text
typeToText typ =
  case typ of
    BooleanType -> "Boolean"
    NumberType -> "Number"
    StringType -> "String"
    ActionType arr ->
      "Action<" <> T.intercalate ", " (typeToText <$> arr) <> ">"
