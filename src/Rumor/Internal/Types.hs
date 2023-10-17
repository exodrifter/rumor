{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}

module Rumor.Internal.Types
  ( Speaker(..)
  , Node(..)
  , Expression(..)
  , simplify
  ) where

import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)
import Data.NonEmptyText (NonEmptyText)
import Data.Scientific (Scientific)

import qualified Data.Maybe as Maybe
import qualified Data.Scientific as S
import qualified Data.Text as T

-- | The identifier for a character who is saying something.
newtype Speaker = Speaker NonEmptyText
  deriving (Eq, Show)

-- | The nodes represent the abstract syntax tree of a Rumor dialog.
data Node =
    Say (Maybe Speaker) (Expression Text)
  | Add (Maybe Speaker) (Expression Text)
  | Control (Expression Bool) (NonEmpty Node) (Maybe (NonEmpty Node))
  | Action0 NonEmptyText
  | Action1 NonEmptyText (Expression Text)
  | Action2 NonEmptyText (Expression Text) (Expression Text)
  | Action3 NonEmptyText (Expression Text) (Expression Text) (Expression Text)
  | Action4 NonEmptyText (Expression Text) (Expression Text) (Expression Text) (Expression Text)
  deriving (Eq, Show)

-- | Represents expressions in a Rumor dialog.
data Expression typ where
  String :: Text -> Expression Text

  Number :: Scientific -> Expression Scientific
  Addition :: Expression Scientific -> Expression Scientific -> Expression Scientific
  Subtraction :: Expression Scientific -> Expression Scientific -> Expression Scientific
  Multiply :: Expression Scientific -> Expression Scientific -> Expression Scientific
  Divide :: Expression Scientific -> Expression Scientific -> Expression Scientific
  NumberToString :: Expression Scientific -> Expression Text

  Boolean :: Bool -> Expression Bool
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
  l <> r =
    let
      toString :: Expression Text -> Text
      toString expression =
        case expression of
          String string ->
            string
          NumberToString fixed ->
            numberToString fixed
          BooleanToString fixed ->
            booleanToString fixed
    in
      String (toString l <> toString r)

instance Monoid (Expression Text) where
  mempty = String ""

simplify :: Expression typ -> Expression typ
simplify expression =
  case expression of
    String _ -> expression

    Number _ -> expression
    Addition _ _ -> Number (calculateNumber expression)
    Subtraction _ _ -> Number (calculateNumber expression)
    Multiply _ _ -> Number (calculateNumber expression)
    Divide _ _ -> Number (calculateNumber expression)
    NumberToString numberExpression -> String (numberToString numberExpression)

    Boolean _ -> expression
    LogicalNot _ -> Boolean (calculateBoolean expression)
    LogicalAnd _ _ -> Boolean (calculateBoolean expression)
    LogicalOr _ _ -> Boolean (calculateBoolean expression)
    LogicalXor _ _ -> Boolean (calculateBoolean expression)
    BooleanToString booleanExpression -> String (booleanToString booleanExpression)

    EqualString _ _ -> Boolean (calculateBoolean expression)
    NotEqualString _ _ -> Boolean (calculateBoolean expression)
    EqualNumber _ _ -> Boolean (calculateBoolean expression)
    NotEqualNumber _ _ -> Boolean (calculateBoolean expression)
    EqualBoolean _ _ -> Boolean (calculateBoolean expression)
    NotEqualBoolean _ _ -> Boolean (calculateBoolean expression)

calculateNumber :: Expression Scientific -> Scientific
calculateNumber expression =
  case expression of
    Number n -> n
    Addition l r -> calculateNumber l + calculateNumber r
    Subtraction l r -> calculateNumber l - calculateNumber r
    Multiply l r -> calculateNumber l * calculateNumber r
    Divide l r -> calculateNumber l / calculateNumber r

calculateBoolean :: Expression Bool -> Bool
calculateBoolean expression =
  case expression of
    Boolean n -> n
    LogicalNot n -> not (calculateBoolean n)
    LogicalAnd l r -> calculateBoolean l && calculateBoolean r
    LogicalOr l r -> calculateBoolean l || calculateBoolean r
    LogicalXor l r -> calculateBoolean l /= calculateBoolean r

    EqualString l r -> l == r
    NotEqualString l r -> l /= r
    EqualNumber l r -> calculateNumber l == calculateNumber r
    NotEqualNumber l r -> calculateNumber l /= calculateNumber r
    EqualBoolean l r -> calculateBoolean l == calculateBoolean r
    NotEqualBoolean l r -> calculateBoolean l /= calculateBoolean r

numberToString :: Expression Scientific -> Text
numberToString expression =
  let
    number = calculateNumber expression
    string = T.pack (S.formatScientific S.Fixed Nothing number)
  in
    Maybe.fromMaybe string (T.stripSuffix ".0" string)

booleanToString :: Expression Bool -> Text
booleanToString expression =
  if calculateBoolean expression
  then "true"
  else "false"
