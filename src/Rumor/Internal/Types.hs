{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}

module Rumor.Internal.Types
  ( Node(..)
  , Expression(..)
  , simplify

  , Type(..)
  , Label(..)
  , Speaker(..)
  , VariableName(..), variableNameToText
  , Unicode(..)
  ) where

import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)
import Data.NonEmptyText (NonEmptyText)
import Data.Scientific (Scientific)

import qualified Data.Maybe as Maybe
import qualified Data.Scientific as S
import qualified Data.NonEmptyText as NET
import qualified Data.Text as T
import qualified Data.Text.ICU.Normalize2 as Normalize

-- $setup
-- >>> import qualified Data.NonEmptyText as NET

data Type = BooleanType | NumberType | StringType

-- | The identifier for a node.
newtype Label = Label Unicode
  deriving (Eq, Show)

-- | The identifier for a character who is saying something.
newtype Speaker = Speaker Unicode
  deriving (Eq, Show)

-- | The name of a variable.
newtype VariableName = VariableName Unicode
  deriving (Eq, Ord, Show)

variableNameToText :: VariableName -> Text
variableNameToText (VariableName (Unicode net)) = NET.toText net

{-| A non-empty unicode string. The string is converted to a normal form when
  testing for equality, but is stored as it was originally written by the
  user.

  >>> Unicode (NET.new 'f' "oo") == Unicode (NET.new 'f' "oo")
  True

  >>> Unicode (NET.new 'f' "oo") == Unicode (NET.new 'b' "ar")
  False

  >>> Unicode (NET.singleton '\225') == Unicode (NET.new 'a' "\769")
  True

  >>> let unwrap (Unicode net) = net
  >>> unwrap (Unicode (NET.singleton '\225')) == unwrap (Unicode (NET.new 'a' "\769"))
  False
-}
newtype Unicode = Unicode NonEmptyText
  deriving Show

instance Eq Unicode where
  (Unicode l) == (Unicode r) =
    Normalize.compareUnicode' (NET.toText l) (NET.toText r) == EQ

instance Ord Unicode where
  compare (Unicode l) (Unicode r) =
    Normalize.compareUnicode' (NET.toText l) (NET.toText r)

-- | The nodes represent the abstract syntax tree of a Rumor dialog.
data Node =
    Say (Maybe Speaker) (Expression Text) (Maybe Label)
  | Add (Maybe Speaker) (Expression Text) (Maybe Label)
  | Control (Expression Bool) (NonEmpty Node) (Maybe (NonEmpty Node))
  | Action0 VariableName
  | Action1 VariableName (Expression Text)
  | Action2 VariableName (Expression Text) (Expression Text)
  | Action3 VariableName (Expression Text) (Expression Text) (Expression Text)
  | Action4 VariableName (Expression Text) (Expression Text) (Expression Text) (Expression Text)
  | Choice (Expression Text) (Maybe Label) (Maybe (NonEmpty Node))
  deriving (Eq, Show)

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
