module Rumor.Internal.Context
( Context, newContext
, getVariableType
, setVariableType
) where

import Data.Text (Text)
import Rumor.Internal.VariableName (VariableName, variableNameToText)
import Rumor.Internal.VariableType (VariableType, typeToText)

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
  VariableName -> VariableType -> Context -> Either Text Context
setVariableType name typ context =
  case getVariableType name context of
    Just existingType
      | existingType == typ ->
          Right context
      | otherwise ->
          Left
            ( "Variable `"
                <> variableNameToText name
                <> "` cannot be a "
                <> typeToText typ
                <> "; it has already been defined as a "
                <> typeToText existingType
                <> "!"
            )
    Nothing ->
      let newVariableTypes = Map.insert name typ (variableTypes context)
      in  Right (Context { variableTypes = newVariableTypes })
