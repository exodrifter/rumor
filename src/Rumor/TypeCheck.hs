module Rumor.TypeCheck
( infer
, check
) where

import Control.Applicative ((<|>))
import Data.Foldable (traverse_)
import Rumor.Parser.Common (Parser, RumorError(..), gets, throw, modifyVariableType)

import qualified Rumor.Internal as Rumor
import qualified Text.Megaparsec as Mega

-- $setup
-- >>> import Data.Either (fromRight)
-- >>> import Data.NonEmptyText as NET
-- >>> import Rumor.Parser.Common
-- >>> import Rumor.Internal
--
-- >>> let foo = Variable (VariableName (Unicode (NET.new 'f' "oo")))
-- >>> let bar = Variable (VariableName (Unicode (NET.new 'b' "ar")))
-- >>> let annotatedFoo b e = AnnotatedVariable b e (VariableName (Unicode (NET.new 'f' "oo")))
-- >>> let annotatedBar b e = AnnotatedVariable b e (VariableName (Unicode (NET.new 'b' "ar")))
-- >>> let parse = parseTest newContext
-- 
-- For tests that succeed, we won't need any annotation information. This eases
-- the writing of such tests by adding fake annotations to an expression.
-- >>> :{
-- fakeAnnotate :: Expression -> AnnotatedExpression
-- fakeAnnotate expression =
--   case expression of
--     Boolean a -> AnnotatedBoolean 0 0 a
--     LogicalNot a -> AnnotatedLogicalNot 0 0 (fakeAnnotate a)
--     LogicalAnd a b -> AnnotatedLogicalAnd 0 0 (fakeAnnotate a) (fakeAnnotate b)
--     LogicalOr a b -> AnnotatedLogicalOr 0 0 (fakeAnnotate a) (fakeAnnotate b)
--     LogicalXor a b -> AnnotatedLogicalXor 0 0 (fakeAnnotate a) (fakeAnnotate b)
--     Number a -> AnnotatedNumber 0 0 a
--     Addition a b -> AnnotatedAddition 0 0 (fakeAnnotate a) (fakeAnnotate b)
--     Subtraction a b -> AnnotatedSubtraction 0 0 (fakeAnnotate a) (fakeAnnotate b)
--     Multiplication a b -> AnnotatedMultiplication 0 0 (fakeAnnotate a) (fakeAnnotate b)
--     Division a b -> AnnotatedDivision 0 0 (fakeAnnotate a) (fakeAnnotate b)
--     String a -> AnnotatedString 0 0 a
--     Concat a b -> AnnotatedConcat 0 0 (fakeAnnotate a) (fakeAnnotate b)
--     Variable a -> AnnotatedVariable 0 0 a
--     Equal a b -> AnnotatedEqual 0 0 (fakeAnnotate a) (fakeAnnotate b)
--     NotEqual a b -> AnnotatedNotEqual 0 0 (fakeAnnotate a) (fakeAnnotate b)
--     ToString a -> AnnotatedToString 0 0 (fakeAnnotate a)
-- :}

--------------------------------------------------------------------------------
-- Type Checking
--------------------------------------------------------------------------------

{-| Infers the type of an expression from the operators or arguments being used.

  For boolean, string, and number operations, we can infer that all of the
  variables must be a boolean, string, or number respectively.

  >>> parse (infer (fakeAnnotate (LogicalNot foo))) "!foo"
  BooleanType
  Context {variableTypes = fromList [(VariableName (Unicode "foo"),BooleanType)]}

  >>> parse (infer (fakeAnnotate (LogicalAnd foo bar))) "foo && bar"
  BooleanType
  Context {variableTypes = fromList [(VariableName (Unicode "bar"),BooleanType),(VariableName (Unicode "foo"),BooleanType)]}

  >>> parse (infer (fakeAnnotate (LogicalOr foo bar))) "foo || bar"
  BooleanType
  Context {variableTypes = fromList [(VariableName (Unicode "bar"),BooleanType),(VariableName (Unicode "foo"),BooleanType)]}

  >>> parse (infer (fakeAnnotate (LogicalXor foo bar))) "foo ^ bar"
  BooleanType
  Context {variableTypes = fromList [(VariableName (Unicode "bar"),BooleanType),(VariableName (Unicode "foo"),BooleanType)]}

  >>> parse (infer (fakeAnnotate (Addition foo bar))) "foo + bar"
  NumberType
  Context {variableTypes = fromList [(VariableName (Unicode "bar"),NumberType),(VariableName (Unicode "foo"),NumberType)]}

  >>> parse (infer (fakeAnnotate (Subtraction foo bar))) "foo - bar"
  NumberType
  Context {variableTypes = fromList [(VariableName (Unicode "bar"),NumberType),(VariableName (Unicode "foo"),NumberType)]}

  >>> parse (infer (fakeAnnotate (Multiplication foo bar))) "foo * bar"
  NumberType
  Context {variableTypes = fromList [(VariableName (Unicode "bar"),NumberType),(VariableName (Unicode "foo"),NumberType)]}

  >>> parse (infer (fakeAnnotate (Division foo bar))) "foo / bar"
  NumberType
  Context {variableTypes = fromList [(VariableName (Unicode "bar"),NumberType),(VariableName (Unicode "foo"),NumberType)]}

  >>> parse (infer (fakeAnnotate (Concat foo bar))) "{foo}{bar}"
  StringType
  Context {variableTypes = fromList [(VariableName (Unicode "bar"),StringType),(VariableName (Unicode "foo"),StringType)]}

  We can infer the type of a variable with an overloaded operator if the other
  argument has a known type.

  >>> parse (infer (fakeAnnotate (Equal foo (Boolean True)))) "foo == true"
  BooleanType
  Context {variableTypes = fromList [(VariableName (Unicode "foo"),BooleanType)]}

  >>> parse (infer (fakeAnnotate (Equal (Boolean True) foo))) "true == foo"
  BooleanType
  Context {variableTypes = fromList [(VariableName (Unicode "foo"),BooleanType)]}

  >>> parse (infer (fakeAnnotate (Equal foo (Number 1)))) "foo == 1"
  BooleanType
  Context {variableTypes = fromList [(VariableName (Unicode "foo"),NumberType)]}

  >>> parse (infer (fakeAnnotate (Equal (Number 1) foo))) "1 == foo"
  BooleanType
  Context {variableTypes = fromList [(VariableName (Unicode "foo"),NumberType)]}

  >>> parse (infer (AnnotatedEqual 4 6 (annotatedFoo 0 3) (annotatedBar 7 10))) "foo == bar"
  1:8:
    |
  1 | foo == bar
    |        ^^^
  Cannot infer type of the variable `bar`.

  >>> parse (infer (AnnotatedEqual 4 6 (annotatedBar 0 3) (annotatedFoo 7 10))) "bar == foo"
  1:8:
    |
  1 | bar == foo
    |        ^^^
  Cannot infer type of the variable `foo`.

  -- TODO: It would be nice if this didn't matter.
  We can only infer the types of variables from left to right.
  >>> parse (infer (fakeAnnotate (LogicalAnd (Equal foo (Number 1)) (Equal foo bar)))) "foo == 1 && foo == bar"
  BooleanType
  Context {variableTypes = fromList [(VariableName (Unicode "bar"),NumberType),(VariableName (Unicode "foo"),NumberType)]}

  >>> parse (infer (AnnotatedLogicalAnd 11 13 (AnnotatedEqual 4 6 (annotatedFoo 0 3) (annotatedBar 7 10)) (AnnotatedEqual 18 20 (annotatedFoo 14 17) (AnnotatedNumber 21 22 1)))) "foo == bar && foo == 1"
  1:8:
    |
  1 | foo == bar && foo == 1
    |        ^^^
  Cannot infer type of the variable `bar`.

  We can only infer the type of an interpolation if the expression within it has
  a known type.

  >>> parse (infer (fakeAnnotate (ToString (Equal foo (Number 1))))) "{foo == 1}"
  StringType
  Context {variableTypes = fromList [(VariableName (Unicode "foo"),NumberType)]}

  >>> parse (infer (AnnotatedToString 0 12 (AnnotatedEqual 5 7 (annotatedFoo 0 3) (annotatedBar 8 11)))) "{foo == bar}"
  1:9:
    |
  1 | {foo == bar}
    |         ^^^
  Cannot infer type of the variable `bar`.

  We can't infer the type of a variable on its own.

  >>> parse (infer (annotatedFoo 0 3)) "foo"
  1:1:
    |
  1 | foo
    | ^^^
  Cannot infer type of the variable `foo`.

  This function will fail if the variables being used already have types
  assigned to them that don't match the inferred use.

  >>> parse (infer (AnnotatedLogicalNot 0 1 (AnnotatedNumber 1 2 1))) "!1"
  1:2:
    |
  1 | !1
    |  ^
  Expected expression to have the type Boolean!

  >>> parse (infer (AnnotatedLogicalAnd 12 14 (AnnotatedEqual 4 6 (annotatedFoo 0 3) (AnnotatedBoolean 7 11 True)) (AnnotatedEqual 19 21 (annotatedFoo 15 18) (AnnotatedNumber 22 23 1)))) "foo == true && foo == 1"
  1:23:
    |
  1 | foo == true && foo == 1
    |                       ^
  Expected expression to have the type Boolean!
-}
infer :: Rumor.AnnotatedExpression -> Parser Rumor.VariableType
infer expression =
  case expression of
    Rumor.AnnotatedBoolean {} ->
      pure Rumor.BooleanType
    Rumor.AnnotatedLogicalNot _ _ a-> do
      check Rumor.BooleanType a
      pure Rumor.BooleanType
    Rumor.AnnotatedLogicalAnd _ _ l r -> do
      traverse_ (check Rumor.BooleanType) [l, r]
      pure Rumor.BooleanType
    Rumor.AnnotatedLogicalOr _ _ l r -> do
      traverse_ (check Rumor.BooleanType) [l, r]
      pure Rumor.BooleanType
    Rumor.AnnotatedLogicalXor _ _ l r -> do
      traverse_ (check Rumor.BooleanType) [l, r]
      pure Rumor.BooleanType

    Rumor.AnnotatedNumber {} ->
      pure Rumor.NumberType
    Rumor.AnnotatedAddition _ _ l r -> do
      traverse_ (check Rumor.NumberType) [l, r]
      pure Rumor.NumberType
    Rumor.AnnotatedSubtraction _ _ l r -> do
      traverse_ (check Rumor.NumberType) [l, r]
      pure Rumor.NumberType
    Rumor.AnnotatedMultiplication _ _ l r -> do
      traverse_ (check Rumor.NumberType) [l, r]
      pure Rumor.NumberType
    Rumor.AnnotatedDivision _ _ l r -> do
      traverse_ (check Rumor.NumberType) [l, r]
      pure Rumor.NumberType

    Rumor.AnnotatedString {} ->
      pure Rumor.StringType
    Rumor.AnnotatedConcat _ _ l r -> do
      traverse_ (check Rumor.StringType) [l, r]
      pure Rumor.StringType

    Rumor.AnnotatedVariable _ _ name -> do
      mVariableType <- gets (Rumor.getVariableType name)
      case mVariableType of
        Just typ -> pure typ
        Nothing -> throw (CannotInferVariable expression name)
    Rumor.AnnotatedEqual _ _ l r -> do
      expected <- infer l <|> infer r
      check expected l
      check expected r
      pure Rumor.BooleanType
    Rumor.AnnotatedNotEqual _ _ l r -> do
      expected <- infer l <|> infer r
      check expected l
      check expected r
      pure Rumor.BooleanType
    Rumor.AnnotatedToString _ _ a -> do
      _ <- infer a
      pure Rumor.StringType

check :: Rumor.VariableType -> Rumor.AnnotatedExpression -> Parser ()
check expected expression =
  case expression of
    Rumor.AnnotatedBoolean {} ->
      if expected == Rumor.BooleanType
      then pure ()
      else throw (TypeMismatch expression expected)
    Rumor.AnnotatedLogicalNot _ _ a -> do
      check Rumor.BooleanType a
    Rumor.AnnotatedLogicalAnd _ _ l r -> do
      check Rumor.BooleanType l
      check Rumor.BooleanType r
    Rumor.AnnotatedLogicalOr _ _ l r -> do
      check Rumor.BooleanType l
      check Rumor.BooleanType r
    Rumor.AnnotatedLogicalXor _ _ l r -> do
      check Rumor.BooleanType l
      check Rumor.BooleanType r

    Rumor.AnnotatedNumber {} ->
      if expected == Rumor.NumberType
      then pure ()
      else throw (TypeMismatch expression expected)
    Rumor.AnnotatedAddition _ _ l r -> do
      check Rumor.NumberType l
      check Rumor.NumberType r
    Rumor.AnnotatedSubtraction _ _ l r -> do
      check Rumor.NumberType l
      check Rumor.NumberType r
    Rumor.AnnotatedMultiplication _ _ l r -> do
      check Rumor.NumberType l
      check Rumor.NumberType r
    Rumor.AnnotatedDivision _ _ l r -> do
      check Rumor.NumberType l
      check Rumor.NumberType r

    Rumor.AnnotatedString {} ->
      if expected == Rumor.StringType
      then pure ()
      else throw (TypeMismatch expression expected)
    Rumor.AnnotatedConcat _ _ l r -> do
      check Rumor.StringType l
      check Rumor.StringType r

    Rumor.AnnotatedVariable _ _ name -> do
      mVariableType <- gets (Rumor.getVariableType name)
      case mVariableType of
        Nothing -> do
          -- TODO: Generate better position information
          pos <- Mega.getOffset
          modifyVariableType name expected pos 0
        Just actual ->
          if expected == actual
          then pure ()
          else throw (TypeMismatch expression actual)
    Rumor.AnnotatedEqual _ _ l r -> do
      inner <- infer l <|> infer r
      check inner l
      check inner r
      if expected == Rumor.BooleanType
      then pure ()
      else throw (TypeMismatch expression Rumor.BooleanType)
    Rumor.AnnotatedNotEqual _ _ l r -> do
      inner <- infer l <|> infer r
      check inner l
      check inner r
      if expected == Rumor.BooleanType
      then pure ()
      else throw (TypeMismatch expression Rumor.BooleanType)
    Rumor.AnnotatedToString _ _ a -> do
      -- If we can infer the type of the interpolation, then we can convert it
      -- to a string.
      _ <- infer a
      if expected == Rumor.StringType
      then pure ()
      else throw (TypeMismatch expression Rumor.StringType)
