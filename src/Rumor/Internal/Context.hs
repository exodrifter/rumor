module Rumor.Internal.Context
( Context, newContext
, getVariableType
, setVariableType
) where

import Rumor.Internal.VariableName (VariableName)
import Rumor.Internal.VariableType (VariableType)

import qualified Data.Map.Strict as Map

newtype Context =
  Context
    { variableTypes :: Map.Map VariableName VariableType
    } deriving (Eq, Show)

newContext :: Context
newContext =
  Context
    { variableTypes = Map.empty
    }

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
