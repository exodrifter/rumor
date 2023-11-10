module Rumor.TypeCheck
( infer
, check
) where

import Control.Applicative ((<|>))
import Data.Foldable (traverse_)
import Rumor.Parser (Parser, RumorError(..), gets, inferenceError, modifyVariableType)

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
-- >>> let parse inner = parseTest newContext (inner <* eof) ""

--------------------------------------------------------------------------------
-- Type Checking
--------------------------------------------------------------------------------

{-| Infers the type of an expression from the operators or arguments being used.

  For boolean, string, and number operations, we can infer that all of the
  variables must be a boolean, string, or number respectively.

  >>> parse (infer (Rumor.LogicalNot foo))
  BooleanType
  Context {variableTypes = fromList [(VariableName (Unicode "foo"),BooleanType)]}

  >>> parse (infer (Rumor.LogicalAnd foo bar))
  BooleanType
  Context {variableTypes = fromList [(VariableName (Unicode "bar"),BooleanType),(VariableName (Unicode "foo"),BooleanType)]}

  >>> parse (infer (Rumor.LogicalOr foo bar))
  BooleanType
  Context {variableTypes = fromList [(VariableName (Unicode "bar"),BooleanType),(VariableName (Unicode "foo"),BooleanType)]}

  >>> parse (infer (Rumor.LogicalXor foo bar))
  BooleanType
  Context {variableTypes = fromList [(VariableName (Unicode "bar"),BooleanType),(VariableName (Unicode "foo"),BooleanType)]}

  >>> parse (infer (Rumor.Addition foo bar))
  NumberType
  Context {variableTypes = fromList [(VariableName (Unicode "bar"),NumberType),(VariableName (Unicode "foo"),NumberType)]}

  >>> parse (infer (Rumor.Subtraction foo bar))
  NumberType
  Context {variableTypes = fromList [(VariableName (Unicode "bar"),NumberType),(VariableName (Unicode "foo"),NumberType)]}

  >>> parse (infer (Rumor.Multiplication foo bar))
  NumberType
  Context {variableTypes = fromList [(VariableName (Unicode "bar"),NumberType),(VariableName (Unicode "foo"),NumberType)]}

  >>> parse (infer (Rumor.Division foo bar))
  NumberType
  Context {variableTypes = fromList [(VariableName (Unicode "bar"),NumberType),(VariableName (Unicode "foo"),NumberType)]}

  >>> parse (infer (Rumor.Concat foo bar))
  StringType
  Context {variableTypes = fromList [(VariableName (Unicode "bar"),StringType),(VariableName (Unicode "foo"),StringType)]}

  We can infer the type of a variable with an overloaded operator if the other
  argument has a known type.

  >>> parse (infer (Equal foo (Number 1)))
  BooleanType
  Context {variableTypes = fromList [(VariableName (Unicode "foo"),NumberType)]}

  >>> parse (infer (Equal (Number 1) foo))
  BooleanType
  Context {variableTypes = fromList [(VariableName (Unicode "foo"),NumberType)]}

  >>> parse (infer (Equal foo (Number 1)))
  BooleanType
  Context {variableTypes = fromList [(VariableName (Unicode "foo"),NumberType)]}

  >>> parse (infer (Equal (Number 1) foo))
  BooleanType
  Context {variableTypes = fromList [(VariableName (Unicode "foo"),NumberType)]}

  >>> parse (infer (Equal foo bar))
  1:1:
    |
  1 | <empty line>
    | ^
  Cannot infer type of `bar`.
  Cannot infer type of `foo`.

  >>> parse (infer (Equal bar foo))
  1:1:
    |
  1 | <empty line>
    | ^
  Cannot infer type of `bar`.
  Cannot infer type of `foo`.

  -- TODO: It would be nice if this didn't matter.
  We can only infer the types of variables from left to right.
  >>> parse (infer (LogicalAnd (Equal foo (Number 1)) (Equal foo bar)))
  BooleanType
  Context {variableTypes = fromList [(VariableName (Unicode "bar"),NumberType),(VariableName (Unicode "foo"),NumberType)]}

  >>> parse (infer (LogicalAnd (Equal foo bar) (Equal foo (Number 1))))
  1:1:
    |
  1 | <empty line>
    | ^
  Cannot infer type of `bar`.
  Cannot infer type of `foo`.

  We can only infer the type of an interpolation if the expression within it has
  a known type.

  >>> parse (infer (ToString (Equal foo (Number 1))))
  StringType
  Context {variableTypes = fromList [(VariableName (Unicode "foo"),NumberType)]}

  >>> parse (infer (ToString (Equal foo bar)))
  1:1:
    |
  1 | <empty line>
    | ^
  Cannot infer type of `bar`.
  Cannot infer type of `foo`.

  We can't infer the type of a variable on its own.

  >>> parse (infer foo)
  1:1:
    |
  1 | <empty line>
    | ^
  Cannot infer type of `foo`.

  This function will fail if the variables being used already have types
  assigned to them that don't match the inferred use.

  >>> parse (infer (LogicalNot (Number 1)))
  1:1:
    |
  1 | <empty line>
    | ^
  Expected a Boolean expression but this expression is actually a Number!

  >>> parse (infer (LogicalAnd (Equal foo (Boolean True)) (Equal foo (Number 1))))
  1:1:
    |
  1 | <empty line>
    | ^
  Expected a Boolean expression but this expression is actually a Number!
-}
infer :: Rumor.Expression -> Parser Rumor.VariableType
infer expression =
  case expression of
    Rumor.Boolean _ ->
      pure Rumor.BooleanType
    Rumor.LogicalNot a -> do
      check Rumor.BooleanType a
      pure Rumor.BooleanType
    Rumor.LogicalAnd l r -> do
      traverse_ (check Rumor.BooleanType) [l, r]
      pure Rumor.BooleanType
    Rumor.LogicalOr l r -> do
      traverse_ (check Rumor.BooleanType) [l, r]
      pure Rumor.BooleanType
    Rumor.LogicalXor l r -> do
      traverse_ (check Rumor.BooleanType) [l, r]
      pure Rumor.BooleanType

    Rumor.Number _ ->
      pure Rumor.NumberType
    Rumor.Addition l r -> do
      traverse_ (check Rumor.NumberType) [l, r]
      pure Rumor.NumberType
    Rumor.Subtraction l r -> do
      traverse_ (check Rumor.NumberType) [l, r]
      pure Rumor.NumberType
    Rumor.Multiplication l r -> do
      traverse_ (check Rumor.NumberType) [l, r]
      pure Rumor.NumberType
    Rumor.Division l r -> do
      traverse_ (check Rumor.NumberType) [l, r]
      pure Rumor.NumberType

    Rumor.String _ ->
      pure Rumor.StringType
    Rumor.Concat l r -> do
      traverse_ (check Rumor.StringType) [l, r]
      pure Rumor.StringType

    Rumor.Variable name -> do
      mVariableType <- gets (Rumor.getVariableType name)
      case mVariableType of
        Just typ -> pure typ
        Nothing -> inferenceError (CannotInferVariable name)
    Rumor.Equal l r -> do
      expected <- infer l <|> infer r
      check expected l
      check expected r
      pure Rumor.BooleanType
    Rumor.NotEqual l r -> do
      expected <- infer l <|> infer r
      check expected l
      check expected r
      pure Rumor.BooleanType
    Rumor.ToString a -> do
      _ <- infer a
      pure Rumor.StringType

check :: Rumor.VariableType -> Rumor.Expression -> Parser ()
check expected expression =
  case expression of
    Rumor.Boolean _ ->
      if expected == Rumor.BooleanType
      then pure ()
      else inferenceError (TypeMismatch expected Rumor.BooleanType)
    Rumor.LogicalNot a -> do
      check Rumor.BooleanType a
    Rumor.LogicalAnd l r -> do
      check Rumor.BooleanType l
      check Rumor.BooleanType r
    Rumor.LogicalOr l r -> do
      check Rumor.BooleanType l
      check Rumor.BooleanType r
    Rumor.LogicalXor l r -> do
      check Rumor.BooleanType l
      check Rumor.BooleanType r

    Rumor.Number _ ->
      if expected == Rumor.NumberType
      then pure ()
      else inferenceError (TypeMismatch expected Rumor.NumberType)
    Rumor.Addition l r -> do
      check Rumor.NumberType l
      check Rumor.NumberType r
    Rumor.Subtraction l r -> do
      check Rumor.NumberType l
      check Rumor.NumberType r
    Rumor.Multiplication l r -> do
      check Rumor.NumberType l
      check Rumor.NumberType r
    Rumor.Division l r -> do
      check Rumor.NumberType l
      check Rumor.NumberType r

    Rumor.String _ ->
      if expected == Rumor.StringType
      then pure ()
      else inferenceError (TypeMismatch expected Rumor.StringType)
    Rumor.Concat l r -> do
      check Rumor.StringType l
      check Rumor.StringType r

    Rumor.Variable name -> do
      mVariableType <- gets (Rumor.getVariableType name)
      case mVariableType of
        Nothing -> do
          -- TODO: Generate better position information
          pos <- Mega.getOffset
          modifyVariableType name expected pos 0
        Just actual ->
          if expected == actual
          then pure ()
          else inferenceError (TypeMismatch expected actual) -- TODO: Can we infer the variable type?
    Rumor.Equal l r -> do
      inner <- infer l <|> infer r
      check inner l
      check inner r
      if expected == Rumor.BooleanType
      then pure ()
      else inferenceError (TypeMismatch expected Rumor.BooleanType) -- TODO: Can we infer the variable type?
    Rumor.NotEqual l r -> do
      inner <- infer l <|> infer r
      check inner l
      check inner r
      if expected == Rumor.BooleanType
      then pure ()
      else inferenceError (TypeMismatch expected Rumor.BooleanType) -- TODO: Can we infer the variable type?
    Rumor.ToString a -> do
      -- If we can infer the type of the interpolation, then we can convert it
      -- to a string.
      _ <- infer a
      if expected == Rumor.StringType
      then pure ()
      else inferenceError (TypeMismatch expected Rumor.StringType)
