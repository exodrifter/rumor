module Rumor.TypeCheck
( infer
, check
) where

import Control.Applicative ((<|>))
import Data.Foldable (traverse_)
import Rumor.Parser (Parser, RumorError(..), gets, throw, modifyVariableType)

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
-- >>> let annotatedFoo = AnnotatedVariable (VariableName (Unicode (NET.new 'f' "oo")))
-- >>> let annotatedBar = AnnotatedVariable (VariableName (Unicode (NET.new 'b' "ar")))
-- >>> let parse = parseTest newContext
-- 
-- For tests that succeed, we won't need any annotation information. This eases
-- the writing of such tests by adding fake annotations to an expression.
-- >>> :{
-- fakeAnnotate :: Expression -> AnnotatedExpression
-- fakeAnnotate expression =
--   case expression of
--     Boolean a -> AnnotatedBoolean a 0 0
--     LogicalNot a -> AnnotatedLogicalNot (fakeAnnotate a) 0 0
--     LogicalAnd a b -> AnnotatedLogicalAnd (fakeAnnotate a) (fakeAnnotate b) 0 0
--     LogicalOr a b -> AnnotatedLogicalOr (fakeAnnotate a) (fakeAnnotate b) 0 0
--     LogicalXor a b -> AnnotatedLogicalXor (fakeAnnotate a) (fakeAnnotate b) 0 0
--     Number a -> AnnotatedNumber a 0 0
--     Addition a b -> AnnotatedAddition (fakeAnnotate a) (fakeAnnotate b) 0 0
--     Subtraction a b -> AnnotatedSubtraction (fakeAnnotate a) (fakeAnnotate b) 0 0
--     Multiplication a b -> AnnotatedMultiplication (fakeAnnotate a) (fakeAnnotate b) 0 0
--     Division a b -> AnnotatedDivision (fakeAnnotate a) (fakeAnnotate b) 0 0
--     String a -> AnnotatedString a 0 0
--     Concat a b -> AnnotatedConcat (fakeAnnotate a) (fakeAnnotate b) 0 0
--     Variable a -> AnnotatedVariable a 0 0
--     Equal a b -> AnnotatedEqual (fakeAnnotate a) (fakeAnnotate b) 0 0
--     NotEqual a b -> AnnotatedNotEqual (fakeAnnotate a) (fakeAnnotate b) 0 0
--     ToString a -> AnnotatedToString (fakeAnnotate a) 0 0
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

  >>> parse (infer (AnnotatedEqual (annotatedFoo 0 3) (annotatedBar 7 10) 0 10)) "foo == bar"
  1:8:
    |
  1 | foo == bar
    |        ^^^
  Cannot infer type of the variable `bar`.

  >>> parse (infer (AnnotatedEqual (annotatedBar 0 3) (annotatedFoo 7 10) 0 10)) "bar == foo"
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

  >>> parse (infer (AnnotatedLogicalAnd (AnnotatedEqual (annotatedFoo 0 3) (annotatedBar 7 10) 0 10) (AnnotatedEqual (annotatedFoo 14 17) (AnnotatedNumber 1 21 22) 14 22) 1 22)) "foo == bar && foo == 1"
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

  >>> parse (infer (AnnotatedToString (AnnotatedEqual (annotatedFoo 0 3) (annotatedBar 8 11) 1 11) 0 12)) "{foo == bar}"
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

  >>> parse (infer (AnnotatedLogicalNot (AnnotatedNumber 1 1 2) 0 2)) "!1"
  1:2:
    |
  1 | !1
    |  ^
  Expected expression to have the type Boolean!

  >>> parse (infer (AnnotatedLogicalAnd (AnnotatedEqual (annotatedFoo 0 3) (AnnotatedBoolean True 7 11) 0 11) (AnnotatedEqual (annotatedFoo 15 18) (AnnotatedNumber 1 22 23) 15 23) 0 23)) "foo == true && foo == 1"
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
    Rumor.AnnotatedLogicalNot a _ _-> do
      check Rumor.BooleanType a
      pure Rumor.BooleanType
    Rumor.AnnotatedLogicalAnd l r _ _ -> do
      traverse_ (check Rumor.BooleanType) [l, r]
      pure Rumor.BooleanType
    Rumor.AnnotatedLogicalOr l r _ _ -> do
      traverse_ (check Rumor.BooleanType) [l, r]
      pure Rumor.BooleanType
    Rumor.AnnotatedLogicalXor l r _ _ -> do
      traverse_ (check Rumor.BooleanType) [l, r]
      pure Rumor.BooleanType

    Rumor.AnnotatedNumber {} ->
      pure Rumor.NumberType
    Rumor.AnnotatedAddition l r _ _ -> do
      traverse_ (check Rumor.NumberType) [l, r]
      pure Rumor.NumberType
    Rumor.AnnotatedSubtraction l r _ _ -> do
      traverse_ (check Rumor.NumberType) [l, r]
      pure Rumor.NumberType
    Rumor.AnnotatedMultiplication l r _ _ -> do
      traverse_ (check Rumor.NumberType) [l, r]
      pure Rumor.NumberType
    Rumor.AnnotatedDivision l r _ _ -> do
      traverse_ (check Rumor.NumberType) [l, r]
      pure Rumor.NumberType

    Rumor.AnnotatedString {} ->
      pure Rumor.StringType
    Rumor.AnnotatedConcat l r _ _ -> do
      traverse_ (check Rumor.StringType) [l, r]
      pure Rumor.StringType

    Rumor.AnnotatedVariable name _ _ -> do
      mVariableType <- gets (Rumor.getVariableType name)
      case mVariableType of
        Just typ -> pure typ
        Nothing -> throw (CannotInferVariable expression name)
    Rumor.AnnotatedEqual l r _ _ -> do
      expected <- infer l <|> infer r
      check expected l
      check expected r
      pure Rumor.BooleanType
    Rumor.AnnotatedNotEqual l r _ _ -> do
      expected <- infer l <|> infer r
      check expected l
      check expected r
      pure Rumor.BooleanType
    Rumor.AnnotatedToString a _ _ -> do
      _ <- infer a
      pure Rumor.StringType

check :: Rumor.VariableType -> Rumor.AnnotatedExpression -> Parser ()
check expected expression =
  case expression of
    Rumor.AnnotatedBoolean {} ->
      if expected == Rumor.BooleanType
      then pure ()
      else throw (TypeMismatch expression expected)
    Rumor.AnnotatedLogicalNot a _ _ -> do
      check Rumor.BooleanType a
    Rumor.AnnotatedLogicalAnd l r _ _ -> do
      check Rumor.BooleanType l
      check Rumor.BooleanType r
    Rumor.AnnotatedLogicalOr l r _ _ -> do
      check Rumor.BooleanType l
      check Rumor.BooleanType r
    Rumor.AnnotatedLogicalXor l r _ _ -> do
      check Rumor.BooleanType l
      check Rumor.BooleanType r

    Rumor.AnnotatedNumber {} ->
      if expected == Rumor.NumberType
      then pure ()
      else throw (TypeMismatch expression expected)
    Rumor.AnnotatedAddition l r _ _ -> do
      check Rumor.NumberType l
      check Rumor.NumberType r
    Rumor.AnnotatedSubtraction l r _ _ -> do
      check Rumor.NumberType l
      check Rumor.NumberType r
    Rumor.AnnotatedMultiplication l r _ _ -> do
      check Rumor.NumberType l
      check Rumor.NumberType r
    Rumor.AnnotatedDivision l r _ _ -> do
      check Rumor.NumberType l
      check Rumor.NumberType r

    Rumor.AnnotatedString {} ->
      if expected == Rumor.StringType
      then pure ()
      else throw (TypeMismatch expression expected)
    Rumor.AnnotatedConcat l r _ _ -> do
      check Rumor.StringType l
      check Rumor.StringType r

    Rumor.AnnotatedVariable name _ _ -> do
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
    Rumor.AnnotatedEqual l r _ _ -> do
      inner <- infer l <|> infer r
      check inner l
      check inner r
      if expected == Rumor.BooleanType
      then pure ()
      else throw (TypeMismatch expression Rumor.BooleanType)
    Rumor.AnnotatedNotEqual l r _ _ -> do
      inner <- infer l <|> infer r
      check inner l
      check inner r
      if expected == Rumor.BooleanType
      then pure ()
      else throw (TypeMismatch expression Rumor.BooleanType)
    Rumor.AnnotatedToString a _ _ -> do
      -- If we can infer the type of the interpolation, then we can convert it
      -- to a string.
      _ <- infer a
      if expected == Rumor.StringType
      then pure ()
      else throw (TypeMismatch expression Rumor.StringType)
