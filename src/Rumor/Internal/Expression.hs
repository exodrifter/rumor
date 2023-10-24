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

  EqualString :: Expression Text -> Expression Text -> Expression Bool
  NotEqualString :: Expression Text -> Expression Text -> Expression Bool
  EqualNumber :: Expression Scientific -> Expression Scientific -> Expression Bool
  NotEqualNumber :: Expression Scientific -> Expression Scientific -> Expression Bool
  EqualBoolean :: Expression Bool -> Expression Bool -> Expression Bool
  NotEqualBoolean :: Expression Bool -> Expression Bool -> Expression Bool

deriving instance Eq (Expression a)
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

simplify :: Expression typ -> Expression typ
simplify expression =
  case expression of
    Concat (String l) (String r) -> String (l <> r)

    Addition (Number l) (Number r) -> Number (l + r)
    Subtraction (Number l) (Number r) -> Number (l - r)
    Division (Number l) (Number r) -> Number (l * r)
    Multiplication (Number l) (Number r) -> Number (l * r)
    NumberToString (Number n) -> String (numberToString n)

    LogicalNot (Boolean b) -> Boolean (not b)
    LogicalAnd (Boolean l) (Boolean r) -> Boolean (l && r)
    LogicalOr (Boolean l) (Boolean r) -> Boolean (l || r)
    LogicalXor (Boolean l) (Boolean r) -> Boolean (l /= r)
    BooleanToString (Boolean b) -> String (booleanToString b)

    EqualString (String l) (String r) -> Boolean (l == r)
    NotEqualString (String l) (String r) -> Boolean (l /= r)
    EqualNumber (Number l) (Number r) -> Boolean (l == r)
    NotEqualNumber (Number l) (Number r) -> Boolean (l /= r)
    EqualBoolean (Boolean l) (Boolean r) -> Boolean (l == r)
    NotEqualBoolean (Boolean l) (Boolean r) -> Boolean (l /= r)

    String _ -> expression
    StringVariable _ -> expression
    Concat _ _ -> expression

    Number _ -> expression
    NumberVariable _ -> expression
    Addition _ _ -> expression
    Subtraction _ _ -> expression
    Division _ _ -> expression
    Multiplication _ _ -> expression
    NumberToString _ -> expression

    Boolean _ -> expression
    BooleanVariable _ -> expression
    LogicalNot _ -> expression
    LogicalAnd _ _ -> expression
    LogicalOr _ _ -> expression
    LogicalXor _ _ -> expression
    BooleanToString _ -> expression

    EqualString _ _ -> expression
    NotEqualString _ _ -> expression
    EqualNumber _ _ -> expression
    NotEqualNumber _ _ -> expression
    EqualBoolean _ _ -> expression
    NotEqualBoolean _ _ -> expression

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
