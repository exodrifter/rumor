{-# LANGUAGE RankNTypes #-}
module Rumor.Internal.Expression
( AnnotatedExpression(..)
, unAnnotate
, annotationBegin
, annotationEnd
, annotationLength
, concatAnnotatedStrings

, Expression(..)
, concatStrings
, expressionToDebugText
) where

import Data.Text (Text)
import Data.Scientific (Scientific)
import Rumor.Internal.VariableName (VariableName, variableNameToText)

import qualified Data.List as List
import qualified Data.Text as T

{-| When parsing, Rumor will parse expressions into this type, which contains
  the expression used by the virtual machine and additional information about
  where the expression is in the source code.
-}
data AnnotatedExpression =
  -- Boolean
    AnnotatedBoolean Int Int Bool
  | AnnotatedLogicalNot Int Int AnnotatedExpression
  | AnnotatedLogicalAnd Int Int AnnotatedExpression AnnotatedExpression
  | AnnotatedLogicalOr Int Int AnnotatedExpression AnnotatedExpression
  | AnnotatedLogicalXor Int Int AnnotatedExpression AnnotatedExpression

  -- Number
  | AnnotatedNumber Int Int Scientific
  | AnnotatedAddition Int Int AnnotatedExpression AnnotatedExpression
  | AnnotatedSubtraction Int Int AnnotatedExpression AnnotatedExpression
  | AnnotatedMultiplication Int Int AnnotatedExpression AnnotatedExpression
  | AnnotatedDivision Int Int AnnotatedExpression AnnotatedExpression

  -- String
  | AnnotatedString Int Int Text
  | AnnotatedConcat Int Int AnnotatedExpression AnnotatedExpression

  -- Overloaded
  | AnnotatedVariable Int Int VariableName
  | AnnotatedEqual Int Int AnnotatedExpression AnnotatedExpression
  | AnnotatedNotEqual Int Int AnnotatedExpression AnnotatedExpression
  | AnnotatedToString Int Int AnnotatedExpression
  deriving (Eq, Ord, Show)

unAnnotate :: AnnotatedExpression -> Expression
unAnnotate annotated =
  case annotated of
    AnnotatedBoolean _ _ a -> Boolean a
    AnnotatedLogicalNot _ _ a -> LogicalNot (unAnnotate a)
    AnnotatedLogicalAnd _ _ a b -> LogicalAnd (unAnnotate a) (unAnnotate b)
    AnnotatedLogicalOr _ _ a b -> LogicalOr (unAnnotate a) (unAnnotate b)
    AnnotatedLogicalXor _ _ a b -> LogicalXor (unAnnotate a) (unAnnotate b)
    AnnotatedNumber _ _ a -> Number a
    AnnotatedAddition _ _ a b -> Addition (unAnnotate a) (unAnnotate b)
    AnnotatedSubtraction _ _ a b -> Subtraction (unAnnotate a) (unAnnotate b)
    AnnotatedMultiplication _ _ a b -> Multiplication (unAnnotate a) (unAnnotate b)
    AnnotatedDivision _ _ a b -> Division (unAnnotate a) (unAnnotate b)
    AnnotatedString _ _ a -> String a
    AnnotatedConcat _ _ a b -> Concat (unAnnotate a) (unAnnotate b)
    AnnotatedVariable _ _ a -> Variable a
    AnnotatedEqual _ _ a b -> Equal (unAnnotate a) (unAnnotate b)
    AnnotatedNotEqual _ _ a b -> NotEqual (unAnnotate a) (unAnnotate b)
    AnnotatedToString _ _ a -> ToString (unAnnotate a)

annotationBegin :: AnnotatedExpression -> Int
annotationBegin annotated =
  case annotated of
    AnnotatedBoolean begin _ _ -> begin
    AnnotatedLogicalNot begin _ _ -> begin
    AnnotatedLogicalAnd begin _ _ _ -> begin
    AnnotatedLogicalOr begin _ _ _ -> begin
    AnnotatedLogicalXor begin _ _ _ -> begin
    AnnotatedNumber begin _ _ -> begin
    AnnotatedAddition begin _ _ _ -> begin
    AnnotatedSubtraction begin _ _ _ -> begin
    AnnotatedMultiplication begin _ _ _ -> begin
    AnnotatedDivision begin _ _ _ -> begin
    AnnotatedString begin _ _ -> begin
    AnnotatedConcat begin _ _ _ -> begin
    AnnotatedVariable begin _ _ -> begin
    AnnotatedEqual begin _ _ _ -> begin
    AnnotatedNotEqual begin _ _ _ -> begin
    AnnotatedToString begin _ _ -> begin

annotationEnd :: AnnotatedExpression -> Int
annotationEnd annotated =
  case annotated of
    AnnotatedBoolean _ end _ -> end
    AnnotatedLogicalNot _ end _ -> end
    AnnotatedLogicalAnd _ end _ _ -> end
    AnnotatedLogicalOr _ end _ _ -> end
    AnnotatedLogicalXor _ end _ _ -> end
    AnnotatedNumber _ end _ -> end
    AnnotatedAddition _ end _ _ -> end
    AnnotatedSubtraction _ end _ _ -> end
    AnnotatedMultiplication _ end _ _ -> end
    AnnotatedDivision _ end _ _ -> end
    AnnotatedString _ end _ -> end
    AnnotatedConcat _ end _ _ -> end
    AnnotatedVariable _ end _ -> end
    AnnotatedEqual _ end _ _ -> end
    AnnotatedNotEqual _ end _ _ -> end
    AnnotatedToString _ end _ -> end

annotationLength :: AnnotatedExpression -> Int
annotationLength annotated =
  case annotated of
    AnnotatedBoolean begin end _ -> end - begin
    AnnotatedLogicalNot begin end _ -> end - begin
    AnnotatedLogicalAnd begin end _ _ -> end - begin
    AnnotatedLogicalOr begin end _ _ -> end - begin
    AnnotatedLogicalXor begin end _ _ -> end - begin
    AnnotatedNumber begin end _ -> end - begin
    AnnotatedAddition begin end _ _ -> end - begin
    AnnotatedSubtraction begin end _ _ -> end - begin
    AnnotatedMultiplication begin end _ _ -> end - begin
    AnnotatedDivision begin end _ _ -> end - begin
    AnnotatedString begin end _ -> end - begin
    AnnotatedConcat begin end _ _ -> end - begin
    AnnotatedVariable begin end _ -> end - begin
    AnnotatedEqual begin end _ _ -> end - begin
    AnnotatedNotEqual begin end _ _ -> end - begin
    AnnotatedToString begin end _ -> end - begin

concatAnnotatedStrings :: Int -> Int -> [AnnotatedExpression] -> AnnotatedExpression
concatAnnotatedStrings start begin expressions =
  let
    step l r =
      AnnotatedConcat (annotationBegin l) (annotationEnd r) l r
  in
    case List.filter (\other -> unAnnotate other /= String "") expressions of
      [] -> AnnotatedString start begin ""
      [expression] -> expression
      _ -> List.foldr1 step expressions

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
  case List.filter (/= String "") expressions of
    [] -> String ""
    [expression] -> expression
    _ -> List.foldr1 Concat expressions

expressionToDebugText :: Expression -> Text
expressionToDebugText expression =
  case expression of
    Boolean True -> "true"
    Boolean False -> "false"
    LogicalNot inner -> "!" <> expressionToDebugText inner
    LogicalAnd l r -> expressionToDebugText l <> " && " <> expressionToDebugText r
    LogicalOr l r -> expressionToDebugText l <> " || " <> expressionToDebugText r
    LogicalXor l r -> expressionToDebugText l <> " ^ " <> expressionToDebugText r

    Number number -> T.pack (show number)
    Addition l r -> expressionToDebugText l <> " + " <> expressionToDebugText r
    Subtraction l r -> expressionToDebugText l <> " - " <> expressionToDebugText r
    Multiplication l r -> expressionToDebugText l <> " * " <> expressionToDebugText r
    Division l r -> expressionToDebugText l <> " / " <> expressionToDebugText r

    String text -> "\"" <> text <> "\""
    Concat l r -> expressionToDebugText l <> " <> " <> expressionToDebugText r

    Variable name -> variableNameToText name
    Equal l r -> expressionToDebugText l <> " == " <> expressionToDebugText r
    NotEqual l r -> expressionToDebugText l <> " /= " <> expressionToDebugText r
    ToString inner -> "{" <> expressionToDebugText inner <> "}"
