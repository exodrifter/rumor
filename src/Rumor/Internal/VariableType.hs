module Rumor.Internal.VariableType
( VariableType(..)
, typeToText
) where

import Data.Text (Text)

data VariableType = BooleanType | NumberType | StringType
  deriving (Eq, Show)

typeToText :: VariableType -> Text
typeToText typ =
  case typ of
    BooleanType -> "Boolean"
    NumberType -> "Number"
    StringType -> "String"
