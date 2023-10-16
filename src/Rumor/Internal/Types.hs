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

import Data.Text (Text)
import Data.Scientific (Scientific)

import qualified Data.Maybe as Maybe
import qualified Data.Scientific as S
import qualified Data.Text as T

-- | The identifier for a character who is saying something.
newtype Speaker = Speaker Text
  deriving (Eq, Show)

-- | The nodes represent the abstract syntax tree of a Rumor dialog.
data Node =
    Say (Maybe Speaker) (Expression Text)
  | Add (Maybe Speaker) (Expression Text)
  | Action0 Text
  | Action1 Text (Expression Text)
  | Action2 Text (Expression Text) (Expression Text)
  | Action3 Text (Expression Text) (Expression Text) (Expression Text)
  | Action4 Text (Expression Text) (Expression Text) (Expression Text) (Expression Text)
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

calculateNumber :: Expression Scientific -> Scientific
calculateNumber expression =
  case expression of
    Number n -> n
    Addition l r -> calculateNumber l + calculateNumber r
    Subtraction l r -> calculateNumber l - calculateNumber r
    Multiply l r -> calculateNumber l * calculateNumber r
    Divide l r -> calculateNumber l / calculateNumber r

numberToString :: Expression Scientific -> Text
numberToString expression =
  let
    number = calculateNumber expression
    string = T.pack (S.formatScientific S.Fixed Nothing number)
  in
    Maybe.fromMaybe string (T.stripSuffix ".0" string)
