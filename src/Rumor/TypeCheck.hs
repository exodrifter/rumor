module Rumor.TypeCheck
( InferenceFailure(..)
, inferenceFailureToText

, infer
, check
) where

import Data.Text (Text)
import Data.Foldable (traverse_)
import qualified Rumor.Internal as Rumor

-- | The types of inference failures that can occur.
data InferenceFailure =
    CannotInferVariable Rumor.VariableName
  | CannotInferExpression
  | TypeMismatch Rumor.VariableType Rumor.VariableType
  | UnexpectedFailure
  deriving Show

inferenceFailureToText :: InferenceFailure -> Text
inferenceFailureToText failure =
  case failure of
    CannotInferVariable name ->
      "Cannot infer type of `" <> Rumor.variableNameToText name <> "`."
    CannotInferExpression ->
      "Cannot infer the type of this expression."
    TypeMismatch expected actual ->
         "Expected a "
      <> Rumor.typeToText expected
      <> " expression but this expression is actually a "
      <> Rumor.typeToText actual
      <> "!"
    UnexpectedFailure ->
      "Unexpected error when inferring expression type! \
      \This is a bug; please report this."

--------------------------------------------------------------------------------
-- Type Checking
--------------------------------------------------------------------------------

infer :: Rumor.Context -> Rumor.Expression -> Either InferenceFailure Rumor.VariableType
infer context expression =
  case expression of
    Rumor.Boolean _ ->
      pure Rumor.BooleanType
    Rumor.LogicalNot a -> do
      check context Rumor.BooleanType a
      pure Rumor.BooleanType
    Rumor.LogicalAnd l r -> do
      traverse_ (check context Rumor.BooleanType) [l, r]
      pure Rumor.BooleanType
    Rumor.LogicalOr l r -> do
      traverse_ (check context Rumor.BooleanType) [l, r]
      pure Rumor.BooleanType
    Rumor.LogicalXor l r -> do
      traverse_ (check context Rumor.BooleanType) [l, r]
      pure Rumor.BooleanType

    Rumor.Number _ ->
      pure Rumor.NumberType
    Rumor.Addition l r -> do
      traverse_ (check context Rumor.NumberType) [l, r]
      pure Rumor.NumberType
    Rumor.Subtraction l r -> do
      traverse_ (check context Rumor.NumberType) [l, r]
      pure Rumor.NumberType
    Rumor.Multiplication l r -> do
      traverse_ (check context Rumor.NumberType) [l, r]
      pure Rumor.NumberType
    Rumor.Division l r -> do
      traverse_ (check context Rumor.NumberType) [l, r]
      pure Rumor.NumberType

    Rumor.String _ ->
      pure Rumor.StringType
    Rumor.Concat l r -> do
      traverse_ (check context Rumor.StringType) [l, r]
      pure Rumor.StringType

    Rumor.Variable name ->
      case Rumor.getVariableType name context of
        Just typ -> pure typ
        Nothing -> Left (CannotInferVariable name)
    Rumor.Equal l r -> do
      case infer context l of
        Left _ ->
          case infer context r of
            Left _ ->
              Left CannotInferExpression
            Right expected  -> do
              check context expected l
              pure expected
        Right expected -> do
          check context expected r
          pure expected
    Rumor.NotEqual l r -> do
      case infer context l of
        Left _ ->
          case infer context r of
            Left _ ->
              Left CannotInferExpression
            Right expected  -> do
              check context expected l
              pure expected
        Right expected -> do
          check context expected r
          pure expected
    Rumor.ToString a -> do
      _ <- infer context a
      pure Rumor.StringType

check :: Rumor.Context -> Rumor.VariableType -> Rumor.Expression -> Either InferenceFailure ()
check context expected expression =
  case expression of
    Rumor.Boolean _ ->
      if expected == Rumor.BooleanType
      then pure ()
      else Left (TypeMismatch expected Rumor.BooleanType)
    Rumor.LogicalNot a -> do
      check context Rumor.BooleanType a
    Rumor.LogicalAnd l r -> do
      check context Rumor.BooleanType l
      check context Rumor.BooleanType r
    Rumor.LogicalOr l r -> do
      check context Rumor.BooleanType l
      check context Rumor.BooleanType r
    Rumor.LogicalXor l r -> do
      check context Rumor.BooleanType l
      check context Rumor.BooleanType r

    Rumor.Number _ ->
      if expected == Rumor.NumberType
      then pure ()
      else Left (TypeMismatch expected Rumor.NumberType)
    Rumor.Addition l r -> do
      check context Rumor.NumberType l
      check context Rumor.NumberType r
    Rumor.Subtraction l r -> do
      check context Rumor.NumberType l
      check context Rumor.NumberType r
    Rumor.Multiplication l r -> do
      check context Rumor.NumberType l
      check context Rumor.NumberType r
    Rumor.Division l r -> do
      check context Rumor.NumberType l
      check context Rumor.NumberType r

    Rumor.String _ ->
      if expected == Rumor.StringType
      then pure ()
      else Left (TypeMismatch expected Rumor.StringType)
    Rumor.Concat l r -> do
      check context Rumor.StringType l
      check context Rumor.StringType r

    Rumor.Variable name -> do
      case Rumor.getVariableType name context of
        Nothing ->
          -- setVariableType name expected context
          pure ()
        Just actual ->
          if expected == actual
          then pure ()
          else Left (TypeMismatch expected actual) -- TODO: Can we infer the variable type?
    Rumor.Equal l r -> do
      case infer context l of
        Left _ ->
          case infer context r of
            Left _ ->
              Left CannotInferExpression
            Right inferredType  -> do
              check context inferredType l
              if expected == Rumor.BooleanType
              then pure ()
              else Left (TypeMismatch expected Rumor.BooleanType)
        Right inferredType -> do
          check context inferredType r
          if expected == Rumor.BooleanType
          then pure ()
          else Left (TypeMismatch expected Rumor.BooleanType)
    Rumor.NotEqual l r -> do
      case infer context l of
        Left _ ->
          case infer context r of
            Left _ ->
              Left CannotInferExpression
            Right inferredType  -> do
              check context inferredType l
              if expected == Rumor.BooleanType
              then pure ()
              else Left (TypeMismatch expected Rumor.BooleanType)
        Right inferredType -> do
          check context inferredType r
          if expected == Rumor.BooleanType
          then pure ()
          else Left (TypeMismatch expected Rumor.BooleanType)
    Rumor.ToString a -> do
      case infer context a of
        Left err -> Left err
        Right _ ->
          if expected == Rumor.StringType
          then pure ()
          else Left (TypeMismatch expected Rumor.StringType)
