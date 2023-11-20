module Rumor.Internal.Context
( Context, newContext
, contextToDebugText
, getVariableType
, setVariableType
) where

import Data.Text (Text)
import Rumor.Internal.VariableName (VariableName, variableNameToText)
import Rumor.Internal.VariableType (VariableType, typeToText)

import qualified Data.Map.Strict as Map
import qualified Data.Text as T

newtype Context =
  Context
    { variableTypes :: Map.Map VariableName VariableType
    } deriving (Eq, Show)

newContext :: Context
newContext =
  Context
    { variableTypes = Map.empty
    }

contextToDebugText :: Context -> Text
contextToDebugText context =
  if context == newContext
  then
    "empty context"
  else
    let
      mapping = Map.toList (variableTypes context)
      toText (name, typ) =
           "let "
        <> variableNameToText name
        <> ": "
        <> typeToText typ
    in
      T.intercalate "\n" (toText <$> mapping) <> "\n"

getVariableType :: VariableName -> Context -> Maybe VariableType
getVariableType name context =
  Map.lookup name (variableTypes context)

setVariableType ::
  VariableName -> VariableType -> Context -> Either VariableType Context
setVariableType name typ context =
  case getVariableType name context of
    Just existingType
      | existingType == typ -> Right context
      | otherwise -> Left existingType
    Nothing ->
      let newVariableTypes = Map.insert name typ (variableTypes context)
      in  Right (Context { variableTypes = newVariableTypes })
