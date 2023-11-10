{-# LANGUAGE RankNTypes #-}
module Rumor.Internal.Expression
( AnnotatedExpression(..)
, unAnnotate
, annotationBegin
, annotationLength

, Expression(..)
, concatStrings
) where

import Data.Text (Text)
import Data.Scientific (Scientific)
import Rumor.Internal.VariableName (VariableName)

import qualified Data.List as List

{-| When parsing, Rumor will parse expressions into this type, which contains
  the expression used by the virtual machine and additional information about
  where the expression is in the source code.
-}
data AnnotatedExpression =
  -- Boolean
    AnnotatedBoolean Bool Int Int
  | AnnotatedLogicalNot AnnotatedExpression Int Int
  | AnnotatedLogicalAnd AnnotatedExpression AnnotatedExpression Int Int
  | AnnotatedLogicalOr AnnotatedExpression AnnotatedExpression Int Int
  | AnnotatedLogicalXor AnnotatedExpression AnnotatedExpression Int Int

  -- Number
  | AnnotatedNumber Scientific Int Int
  | AnnotatedAddition AnnotatedExpression AnnotatedExpression Int Int
  | AnnotatedSubtraction AnnotatedExpression AnnotatedExpression Int Int
  | AnnotatedMultiplication AnnotatedExpression AnnotatedExpression Int Int
  | AnnotatedDivision AnnotatedExpression AnnotatedExpression Int Int

  -- String
  | AnnotatedString Text Int Int
  | AnnotatedConcat AnnotatedExpression AnnotatedExpression Int Int

  -- Overloaded
  | AnnotatedVariable VariableName Int Int
  | AnnotatedEqual AnnotatedExpression AnnotatedExpression Int Int
  | AnnotatedNotEqual AnnotatedExpression AnnotatedExpression Int Int
  | AnnotatedToString AnnotatedExpression Int Int
  deriving (Eq, Ord, Show)

unAnnotate :: AnnotatedExpression -> Expression
unAnnotate annotated =
  case annotated of
    AnnotatedBoolean a _ _ -> Boolean a
    AnnotatedLogicalNot a _ _ -> LogicalNot (unAnnotate a)
    AnnotatedLogicalAnd a b _ _ -> LogicalAnd (unAnnotate a) (unAnnotate b)
    AnnotatedLogicalOr a b _ _ -> LogicalOr (unAnnotate a) (unAnnotate b)
    AnnotatedLogicalXor a b _ _ -> LogicalXor (unAnnotate a) (unAnnotate b)
    AnnotatedNumber a _ _ -> Number a
    AnnotatedAddition a b _ _ -> Addition (unAnnotate a) (unAnnotate b)
    AnnotatedSubtraction a b _ _ -> Subtraction (unAnnotate a) (unAnnotate b)
    AnnotatedMultiplication a b _ _ -> Multiplication (unAnnotate a) (unAnnotate b)
    AnnotatedDivision a b _ _ -> Division (unAnnotate a) (unAnnotate b)
    AnnotatedString a _ _ -> String a
    AnnotatedConcat a b _ _ -> Concat (unAnnotate a) (unAnnotate b)
    AnnotatedVariable a _ _ -> Variable a
    AnnotatedEqual a b _ _ -> Equal (unAnnotate a) (unAnnotate b)
    AnnotatedNotEqual a b _ _ -> NotEqual (unAnnotate a) (unAnnotate b)
    AnnotatedToString a _ _ -> ToString (unAnnotate a)

annotationBegin :: AnnotatedExpression -> Int
annotationBegin annotated =
  case annotated of
    AnnotatedBoolean _ begin _ -> begin
    AnnotatedLogicalNot _ begin _ -> begin
    AnnotatedLogicalAnd _ _ begin _ -> begin
    AnnotatedLogicalOr _ _ begin _ -> begin
    AnnotatedLogicalXor _ _ begin _ -> begin
    AnnotatedNumber _ begin _ -> begin
    AnnotatedAddition _ _ begin _ -> begin
    AnnotatedSubtraction _ _ begin _ -> begin
    AnnotatedMultiplication _ _ begin _ -> begin
    AnnotatedDivision _ _ begin _ -> begin
    AnnotatedString _ begin _ -> begin
    AnnotatedConcat _ _ begin _ -> begin
    AnnotatedVariable _ begin _ -> begin
    AnnotatedEqual _ _ begin _ -> begin
    AnnotatedNotEqual _ _ begin _ -> begin
    AnnotatedToString _ begin _ -> begin

annotationLength :: AnnotatedExpression -> Int
annotationLength annotated =
  case annotated of
    AnnotatedBoolean _ begin end -> end - begin
    AnnotatedLogicalNot _ begin end -> end - begin
    AnnotatedLogicalAnd _ _ begin end -> end - begin
    AnnotatedLogicalOr _ _ begin end -> end - begin
    AnnotatedLogicalXor _ _ begin end -> end - begin
    AnnotatedNumber _ begin end -> end - begin
    AnnotatedAddition _ _ begin end -> end - begin
    AnnotatedSubtraction _ _ begin end -> end - begin
    AnnotatedMultiplication _ _ begin end -> end - begin
    AnnotatedDivision _ _ begin end -> end - begin
    AnnotatedString _ begin end -> end - begin
    AnnotatedConcat _ _ begin end -> end - begin
    AnnotatedVariable _ begin end -> end - begin
    AnnotatedEqual _ _ begin end -> end - begin
    AnnotatedNotEqual _ _ begin end -> end - begin
    AnnotatedToString _ begin end -> end - begin

{-| An expression which is loosely typed -- the types of the variables are not
  known.
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
