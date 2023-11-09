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

--------------------------------------------------------------------------------
-- Type Checking
--------------------------------------------------------------------------------

{-| Infers the type of an expression from the operators or arguments being used.

  For boolean, string, and number operations, we can infer that all of the
  variables must be a boolean, string, or number respectively.

  >>> parseTest context (infer (Rumor.LogicalNot foo)) ""
  BooleanType

  >>> parseTest context (infer (Rumor.LogicalAnd foo bar)) ""
  BooleanType

  >>> parseTest context (infer (Rumor.LogicalOr foo bar)) ""
  BooleanType

  >>> parseTest context (infer (Rumor.LogicalXor foo bar)) ""
  BooleanType

  >>> parseTest context (infer (Rumor.Addition foo bar)) ""
  NumberType

  >>> parseTest context (infer (Rumor.Subtraction foo bar)) ""
  NumberType

  >>> parseTest context (infer (Rumor.Multiplication foo bar)) ""
  NumberType

  >>> parseTest context (infer (Rumor.Division foo bar)) ""
  NumberType

  >>> parseTest context (infer (Rumor.Concat foo bar)) ""
  StringType

  We can infer the type of a variable with an overloaded operator if the other
  argument has a known type.

  >>> parseTest context (infer (Equal foo number)) ""
  BooleanType

  >>> parseTest context (infer (Equal number foo)) ""
  BooleanType

  >>> parseTest context (infer (Equal foo number)) ""
  BooleanType

  >>> parseTest context (infer (Equal number foo)) ""
  BooleanType

  >>> parseTest context (infer (Equal foo bar)) ""
  1:1:
    |
  1 | <empty line>
    | ^
  Cannot infer type of `bar`.
  Cannot infer type of `foo`.

  >>> parseTest context (infer (Equal bar foo)) ""
  1:1:
    |
  1 | <empty line>
    | ^
  Cannot infer type of `bar`.
  Cannot infer type of `foo`.

  We can only infer the type of an interpolation if the expression within it has
  a known type.

  >>> parseTest context (infer (ToString (Equal foo number))) ""
  StringType

  >>> parseTest context (infer (ToString (Equal foo bar))) ""
  1:1:
    |
  1 | <empty line>
    | ^
  Cannot infer type of `bar`.
  Cannot infer type of `foo`.

  We can't infer the type of a variable on its own.

  >>> parseTest context (infer foo) ""
  1:1:
    |
  1 | <empty line>
    | ^
  Cannot infer type of `foo`.

  This function will fail if the variables being used already have types
  assigned to them that don't match the inferred use.

  >>> parseTest context (infer (LogicalNot string)) ""
  1:1:
    |
  1 | <empty line>
    | ^
  Expected a Boolean expression but this expression is actually a String!

  >>> parseTest context (infer (LogicalAnd (Equal foo string) (Equal foo number))) ""
  1:1:
    |
  1 | <empty line>
    | ^
  Expected a String expression but this expression is actually a Number!
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
