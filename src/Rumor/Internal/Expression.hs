{-# LANGUAGE RankNTypes #-}
module Rumor.Internal.Expression
( Expression(..)
, concatStrings

, infer
, check
, InferenceFailure(..)
, inferenceFailureToText
) where

import Data.Text (Text)
import Data.Scientific (Scientific)
import Rumor.Internal.VariableName (VariableName, variableNameToText)
import Rumor.Internal.VariableType (VariableType(..), typeToText)
import Rumor.Internal.Context (Context, getVariableType)

import qualified Data.List as List

-- $setup
-- >>> import Data.Either (fromRight)
-- >>> import Data.NonEmptyText as NET
-- >>> import Rumor.Internal
-- >>> import Rumor.Internal.Unicode
-- >>> import Rumor.Internal.VariableName
-- >>> import Rumor.Parser.Common
--
-- >>> :{
-- let setVariableTypes c0 = do
--       c1 <- setVariableType (VariableName (Unicode (NET.new 's' "tring"))) StringType c0
--       c2 <- setVariableType (VariableName (Unicode (NET.new 'n' "umber"))) NumberType c1
--       c3 <- setVariableType (VariableName (Unicode (NET.new 'b' "oolean"))) BooleanType c2
--       pure c3
-- :}
--
-- >>> let context = fromRight undefined (setVariableTypes newContext)
-- >>> let foo = Variable (VariableName (Unicode (NET.new 'f' "oo")))
-- >>> let bar = Variable (VariableName (Unicode (NET.new 'b' "ar")))
-- >>> let string = Variable (VariableName (Unicode (NET.new 's' "tring")))
-- >>> let number = Variable (VariableName (Unicode (NET.new 'n' "umber")))
-- >>> let boolean = Variable (VariableName (Unicode (NET.new 'b' "oolean")))

{-| When parsing, Rumor will parse expressions into this type, which is an
  expression which is loosely typed -- the types of the variables are not known.

  This is lets us do the parsing step separately from the type inference step.
-}
data Expression =
  -- Boolean
    Boolean Bool
  | LogicalNot Expression
  | LogicalAnd Expression Expression
  | LogicalOr Expression Expression
  | LogicalXor Expression Expression

  -- Number
  | Number Scientific
  | Addition Expression Expression
  | Subtraction Expression Expression
  | Multiplication Expression Expression
  | Division Expression Expression

  -- String
  | String Text
  | Concat Expression Expression

  -- Overloaded
  | Variable VariableName
  | Equal Expression Expression
  | NotEqual Expression Expression
  | ToString Expression
  deriving (Eq, Show)

concatStrings :: [Expression] -> Expression
concatStrings expressions =
  let
    step lhs rhs =
      case (lhs, rhs) of
        (String l, String r) ->
          String (l <> r)
        (String l, Concat (String r1) r2) ->
          Concat (String (l <> r1)) r2
        (Concat l1 (String l2), String r) ->
          Concat l1 (String (l2 <> r))
        (Concat l1 (String l2), Concat (String r1) r2) ->
          Concat l1 (Concat (String (l2 <> r1)) r2)
        (l, r) -> Concat l r
  in
    case List.filter ((/=) (String "")) expressions of
      [] -> String ""
      [expression] -> expression
      _ -> List.foldr1 step expressions

--------------------------------------------------------------------------------
-- Inference
--------------------------------------------------------------------------------

infer :: Context -> Expression -> Either InferenceFailure VariableType
infer context expression =
  case expression of
    Boolean _ ->
      pure BooleanType
    LogicalNot a -> do
      check context BooleanType a
      pure BooleanType
    LogicalAnd l r -> do
      _ <- traverse (check context BooleanType) [l, r]
      pure BooleanType
    LogicalOr l r -> do
      _ <- traverse (check context BooleanType) [l, r]
      pure BooleanType
    LogicalXor l r -> do
      _ <- traverse (check context BooleanType) [l, r]
      pure BooleanType

    Number _ ->
      pure NumberType
    Addition l r -> do
      _ <- traverse (check context NumberType) [l, r]
      pure NumberType
    Subtraction l r -> do
      _ <- traverse (check context NumberType) [l, r]
      pure NumberType
    Multiplication l r -> do
      _ <- traverse (check context NumberType) [l, r]
      pure NumberType
    Division l r -> do
      _ <- traverse (check context NumberType) [l, r]
      pure NumberType

    String _ ->
      pure StringType
    Concat l r -> do
      _ <- traverse (check context StringType) [l, r]
      pure StringType

    Variable name ->
      case getVariableType name context of
        Just typ -> pure typ
        Nothing -> Left (CannotInferVariable name)
    Equal l r -> do
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
    NotEqual l r -> do
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
    ToString a -> do
      _ <- infer context a
      pure StringType

check :: Context -> VariableType -> Expression -> Either InferenceFailure ()
check context expected expression =
  case expression of
    Boolean _ ->
      if expected == BooleanType
      then pure ()
      else Left (TypeMismatch expected BooleanType)
    LogicalNot a -> do
      check context BooleanType a
    LogicalAnd l r -> do
      check context BooleanType l
      check context BooleanType r
    LogicalOr l r -> do
      check context BooleanType l
      check context BooleanType r
    LogicalXor l r -> do
      check context BooleanType l
      check context BooleanType r

    Number _ ->
      if expected == NumberType
      then pure ()
      else Left (TypeMismatch expected NumberType)
    Addition l r -> do
      check context NumberType l
      check context NumberType r
    Subtraction l r -> do
      check context NumberType l
      check context NumberType r
    Multiplication l r -> do
      check context NumberType l
      check context NumberType r
    Division l r -> do
      check context NumberType l
      check context NumberType r

    String _ ->
      if expected == StringType
      then pure ()
      else Left (TypeMismatch expected StringType)
    Concat l r -> do
      check context StringType l
      check context StringType r

    Variable name -> do
      case getVariableType name context of
        Nothing ->
          -- setVariableType name expected context
          pure ()
        Just actual ->
          if expected == actual
          then pure ()
          else Left (TypeMismatch expected actual) -- TODO: Can we infer the variable type?
    Equal l r -> do
      case infer context l of
        Left _ ->
          case infer context r of
            Left _ ->
              Left CannotInferExpression
            Right inferredType  -> do
              check context inferredType l
              if expected == BooleanType
              then pure ()
              else Left (TypeMismatch expected BooleanType)
        Right inferredType -> do
          check context inferredType r
          if expected == BooleanType
          then pure ()
          else Left (TypeMismatch expected BooleanType)
    NotEqual l r -> do
      case infer context l of
        Left _ ->
          case infer context r of
            Left _ ->
              Left CannotInferExpression
            Right inferredType  -> do
              check context inferredType l
              if expected == BooleanType
              then pure ()
              else Left (TypeMismatch expected BooleanType)
        Right inferredType -> do
          check context inferredType r
          if expected == BooleanType
          then pure ()
          else Left (TypeMismatch expected BooleanType)
    ToString a -> do
      case infer context a of
        Left err -> Left err
        Right _ ->
          if expected == StringType
          then pure ()
          else Left (TypeMismatch expected StringType)

-- | The types of inference failures that can occur.
data InferenceFailure =
    CannotInferVariable VariableName
  | CannotInferExpression
  | TypeMismatch VariableType VariableType
  | UnexpectedFailure
  deriving Show

inferenceFailureToText :: InferenceFailure -> Text
inferenceFailureToText failure =
  case failure of
    CannotInferVariable name ->
      "Cannot infer type of `" <> variableNameToText name <> "`."
    CannotInferExpression ->
      "Cannot infer the type of this expression."
    TypeMismatch expected actual ->
         "Expected a "
      <> typeToText expected
      <> " expression but this expression is actually a "
      <> typeToText actual
      <> "!"
    UnexpectedFailure ->
      "Unexpected error when inferring expression type! \
      \This is a bug; please report this."
