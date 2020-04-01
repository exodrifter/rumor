{-# LANGUAGE GADTs #-}
{-# LANGUAGE Safe #-}

module Rumor.Expression.Type
( Expression(..)
, evaluate
, evaluateBoolean
, evaluateNumber
, evaluateText
, simplify
, simplifyBoolean
, simplifyNumber
, simplifyText
) where

import Rumor.Prelude
import Rumor.Value (Value(..))
import qualified Data.Char as Char
import qualified Data.Text as T

data Expression a where
  Boolean :: Bool -> Expression Bool

  Number :: Double -> Expression Double

  Text :: T.Text -> Expression T.Text
  Concat :: Expression T.Text -> Expression T.Text -> Expression T.Text
  NumberSubstitution :: Expression Double -> Expression T.Text
  BooleanSubstitution :: Expression Bool -> Expression T.Text

instance Show (Expression a) where
  show expr =
    case expr of
      Boolean a -> "(Boolean " ++ show a ++ ")"

      Number a -> "(Number " ++ show a ++ ")"

      Text a -> "(Text " ++ show a ++ ")"
      Concat a b -> "(Concat " ++ show a ++ " " ++ show b ++ ")"
      NumberSubstitution a -> "(NumberSubstitution" ++ show a ++ ")"
      BooleanSubstitution a -> "(BooleanSubstitution" ++ show a ++ ")"

instance Eq (Expression a) where
  (==) l r =
    case (l, r) of
      (Number a, Number b) -> a == b

      (Boolean a, Boolean b) -> a == b

      (Text a, Text b) -> a == b
      (Concat a b, Concat c d) -> a == c && b == d
      (NumberSubstitution a, NumberSubstitution b) -> a == b
      (BooleanSubstitution a, BooleanSubstitution b) -> a == b

      (_, _) -> False

-- Defines Rumor's conversion from a value produced by a substitution to
-- text
class SubstitutionText a where
  toText :: a -> T.Text

instance SubstitutionText Bool where
  toText a =
    case a of
      True -> "true"
      False -> "false"

instance SubstitutionText Double where
  toText = T.pack . show

-- Evaluates an expression
evaluate :: Expression a -> Value
evaluate expr =
  case expr of
    Boolean _ -> BooleanValue $ evaluateBoolean expr

    Number _ -> NumberValue $ evaluateNumber expr

    Text _ -> TextValue $ evaluateText expr
    Concat _ _ -> TextValue $ evaluateText expr
    NumberSubstitution _ -> TextValue $ evaluateText expr
    BooleanSubstitution _ -> TextValue $ evaluateText expr

evaluateBoolean :: Expression Bool -> Bool
evaluateBoolean expr =
  case expr of
    Boolean a -> a

evaluateNumber :: Expression Double -> Double
evaluateNumber expr =
  case expr of
    Number a -> a

evaluateText :: Expression T.Text -> T.Text
evaluateText expr =
  case expr of
    Text a -> a
    Concat a b ->
      evaluateText a <> evaluateText b
    NumberSubstitution a ->
      toText $ evaluateNumber a
    BooleanSubstitution a ->
      toText $ evaluateBoolean a

-- Simplifies an expression
simplify :: Expression a -> Expression a
simplify expr =
  case expr of
    Boolean _ -> simplifyBoolean expr

    Number _ -> simplifyNumber expr

    Text _ -> simplifyText expr
    Concat _ _ -> simplifyText expr
    NumberSubstitution _ -> simplifyText expr
    BooleanSubstitution _ -> simplifyText expr

simplifyBoolean :: Expression Bool -> Expression Bool
simplifyBoolean expr =
  case expr of
    Boolean _ -> expr

simplifyNumber :: Expression Double -> Expression Double
simplifyNumber expr =
  case expr of
    Number _ -> expr

simplifyText :: Expression T.Text -> Expression T.Text
simplifyText expr =
  case expr of
    Text a ->
      Text . collapseSpaces $ a

    Concat (Text l) (Text r) ->
      Text . collapseSpaces $ l <> r
    Concat (Text l) (NumberSubstitution (Number r)) ->
      Text . collapseSpaces $ l <> toText r
    Concat (Text l) (BooleanSubstitution (Boolean r)) ->
      Text . collapseSpaces $ l <> toText r
    Concat (NumberSubstitution (Number l)) (Text r) ->
      Text . collapseSpaces $ toText l <> r
    Concat (NumberSubstitution (Number l)) (NumberSubstitution (Number r)) ->
      Text . collapseSpaces $ toText l <> toText r
    Concat (NumberSubstitution (Number l)) (BooleanSubstitution (Boolean r)) ->
      Text . collapseSpaces $ toText l <> toText r
    Concat (BooleanSubstitution (Boolean l)) (Text r) ->
      Text . collapseSpaces $ toText l <> r
    Concat (BooleanSubstitution (Boolean l)) (NumberSubstitution (Number r)) ->
      Text . collapseSpaces $ toText l <> toText r
    Concat (BooleanSubstitution (Boolean l)) (BooleanSubstitution (Boolean r)) ->
      Text . collapseSpaces $ toText l <> toText r
    Concat l r ->
      if Concat (simplifyText l) (simplifyText r) == expr
      then expr
      else simplifyText $ Concat (simplifyText l) (simplifyText r)

    NumberSubstitution (Number a) ->
      Text . collapseSpaces $ toText a
    BooleanSubstitution (Boolean a) ->
      Text . collapseSpaces $ toText a

collapseSpaces :: T.Text -> T.Text
collapseSpaces =
  flip T.foldl' "" $ \txt ch ->
    case T.unsnoc txt of
      -- Our string is empty
      Nothing ->
        if Char.isSpace ch
        then T.singleton ' '
        else T.singleton ch

      -- Our string is not empty
      Just (_, previousCharacter) ->
        case (Char.isSpace previousCharacter, Char.isSpace ch) of
          (True, True) -> txt
          (True, False) -> T.snoc txt ch
          (False, True) -> T.snoc txt ' '
          (False, False) -> T.snoc txt ch
