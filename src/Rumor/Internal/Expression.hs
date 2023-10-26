{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}

module Rumor.Internal.Expression
( Expression(..)
, simplify
) where

import Data.Text (Text)
import Data.Scientific (Scientific)
import Rumor.Internal.VariableName (VariableName)

import qualified Data.Maybe as Maybe
import qualified Data.Scientific as S
import qualified Data.Text as T

-- $setup
-- >>> import qualified Data.NonEmptyText as NET
-- >>> import Rumor.Internal.VariableName
-- >>> import Rumor.Internal.Unicode
-- >>> let foo = VariableName (Unicode (NET.new 'f' "oo"))

-- | Represents expressions in a Rumor dialog.
data Expression typ where
  String :: Text -> Expression Text
  StringVariable :: VariableName -> Expression Text
  Concat :: Expression Text -> Expression Text -> Expression Text

  Number :: Scientific -> Expression Scientific
  NumberVariable :: VariableName -> Expression Scientific
  Addition :: Expression Scientific -> Expression Scientific -> Expression Scientific
  Subtraction :: Expression Scientific -> Expression Scientific -> Expression Scientific
  Multiplication :: Expression Scientific -> Expression Scientific -> Expression Scientific
  Division :: Expression Scientific -> Expression Scientific -> Expression Scientific
  NumberToString :: Expression Scientific -> Expression Text

  Boolean :: Bool -> Expression Bool
  BooleanVariable :: VariableName -> Expression Bool
  LogicalNot :: Expression Bool -> Expression Bool
  LogicalAnd :: Expression Bool -> Expression Bool -> Expression Bool
  LogicalOr :: Expression Bool -> Expression Bool -> Expression Bool
  LogicalXor :: Expression Bool -> Expression Bool -> Expression Bool
  BooleanToString :: Expression Bool -> Expression Text

  Equal :: Expression a -> Expression a -> Expression Bool
  NotEqual :: Expression a -> Expression a -> Expression Bool

deriving instance Show (Expression a)

instance Semigroup (Expression Text) where
  String l <> String r =
    String (l <> r)
  String l <> (Concat (String r1) r2) =
    Concat (String (l <> r1)) r2
  (Concat l1 (String l2)) <> String r =
    Concat l1 (String (l2 <> r))
  (Concat l1 (String l2)) <> (Concat (String r1) r2) =
    Concat l1 (Concat (String (l2 <> r1)) r2)
  l <> r = Concat l r

instance Monoid (Expression Text) where
  mempty = String ""
  mconcat arr =
    case arr of
      [] -> mempty
      [x] -> x
      x:rest -> x <> mconcat rest

{-| Simplifies expressions until they can no longer be reduced.

  -- TODO: This simplification logic could be better.

  >>> simplify (LogicalAnd (Boolean True) (Boolean True))
  Boolean True

  >>> simplify (LogicalAnd (LogicalAnd (Boolean True) (Boolean True)) (LogicalAnd (Boolean True) (Boolean True)))
  Boolean True

  >>> simplify (Addition (Number 1) (Number 2))
  Number 3.0

  >>> simplify (Addition (Addition (Number 1) (Number 2)) (Addition (Number 3) (Number 4)))
  Number 10.0

  >>> simplify (Concat (String "1") (String "2"))
  String "12"

  >>> simplify (Concat (Concat (String "1") (String "2")) (Concat (String "3") (String "4")))
  String "1234"

  It cannot simplify expressions containing variables.

  >>> simplify (LogicalAnd (BooleanVariable foo) (Boolean True))
  LogicalAnd (BooleanVariable (VariableName (Unicode "foo"))) (Boolean True)

  >>> simplify (LogicalAnd (LogicalAnd (BooleanVariable foo) (Boolean True)) (LogicalAnd (Boolean True) (Boolean True)))
  LogicalAnd (LogicalAnd (BooleanVariable (VariableName (Unicode "foo"))) (Boolean True)) (Boolean True)

  >>> simplify (Addition (NumberVariable foo) (Number 2))
  Addition (NumberVariable (VariableName (Unicode "foo"))) (Number 2.0)

  >>> simplify (Addition (Addition (NumberVariable foo) (Number 2)) (Addition (Number 3) (Number 4)))
  Addition (Addition (NumberVariable (VariableName (Unicode "foo"))) (Number 2.0)) (Number 7.0)

  >>> simplify (Concat (StringVariable foo) (String "2"))
  Concat (StringVariable (VariableName (Unicode "foo"))) (String "2")

  >>> simplify (Concat (Concat (StringVariable foo) (String "2")) (Concat (String "3") (String "4")))
  Concat (Concat (StringVariable (VariableName (Unicode "foo"))) (String "2")) (String "34")

-}
simplify :: Expression typ -> Expression typ
simplify expression =
  case expression of
    String _ -> expression
    StringVariable _ -> expression
    Concat lhs rhs ->
      case (simplify lhs, simplify rhs) of
        (String l, String r) -> String (l <> r)
        (l, r) -> Concat l r

    Number _ -> expression
    NumberVariable _ -> expression
    Addition lhs rhs ->
      case (simplify lhs, simplify rhs) of
        (Number l, Number r) -> Number (l + r)
        (l, r) -> Addition l r
    Subtraction lhs rhs ->
      case (simplify lhs, simplify rhs) of
        (Number l, Number r) -> Number (l + r)
        (l, r) -> Subtraction l r
    Multiplication lhs rhs ->
      case (simplify lhs, simplify rhs) of
        (Number l, Number r) -> Number (l + r)
        (l, r) -> Multiplication l r
    Division lhs rhs ->
      case (simplify lhs, simplify rhs) of
        (Number l, Number r) -> Number (l + r)
        (l, r) -> Division l r
    NumberToString inner ->
      case simplify inner of
        Number n -> String (numberToString n)
        n -> NumberToString n

    Boolean _ -> expression
    BooleanVariable _ -> expression
    LogicalNot inner ->
      case simplify inner of
        Boolean b -> Boolean (not b)
        b -> LogicalNot b
    LogicalAnd lhs rhs ->
      case (simplify lhs, simplify rhs) of
        (Boolean l, Boolean r) -> Boolean (l && r)
        (l, r) -> LogicalAnd l r
    LogicalOr lhs rhs ->
      case (simplify lhs, simplify rhs) of
        (Boolean l, Boolean r) -> Boolean (l || r)
        (l, r) -> LogicalOr l r
    LogicalXor lhs rhs ->
      case (simplify lhs, simplify rhs) of
        (Boolean l, Boolean r) -> Boolean (l /= r)
        (l, r) -> LogicalXor l r
    BooleanToString inner ->
      case simplify inner of
        Boolean b -> String (booleanToString b)
        b -> BooleanToString b

    Equal lhs rhs ->
      case (simplify lhs, simplify rhs) of
        (String l, String r) -> Boolean (l == r)
        (Number l, Number r) -> Boolean (l == r)
        (Boolean l, Boolean r) -> Boolean (l == r)
        (l, r) -> Equal l r
    NotEqual lhs rhs ->
      case (simplify lhs, simplify rhs) of
        (String l, String r) -> Boolean (l /= r)
        (Number l, Number r) -> Boolean (l /= r)
        (Boolean l, Boolean r) -> Boolean (l /= r)
        (l, r) -> NotEqual l r

numberToString :: Scientific -> Text
numberToString number =
  let
    text = T.pack (S.formatScientific S.Fixed Nothing number)
  in
    Maybe.fromMaybe text (T.stripSuffix ".0" text)

booleanToString :: Bool -> Text
booleanToString bool =
  if bool
  then "true"
  else "false"
