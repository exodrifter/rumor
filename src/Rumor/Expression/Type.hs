{-# LANGUAGE GADTs #-}

module Rumor.Expression.Type
( Expression(..)
, evaluate
, evaluateBoolean
, evaluateNumber
, evaluateText
, simplify
, simplifyBoolean
, simplifyMath
, simplifyText
) where

import Rumor.Value (Value(..))

import qualified Data.Char as Char
import qualified Data.Text as T

data Expression r a where
  Boolean :: Bool -> Expression r Bool

  Number :: Fixed r -> Expression r (Fixed r)
  Add :: Expression r (Fixed r) -> Expression r (Fixed r) -> Expression r (Fixed r)
  Subtract :: Expression r (Fixed r) -> Expression r (Fixed r) -> Expression r (Fixed r)
  Multiply :: Expression r (Fixed r) -> Expression r (Fixed r) -> Expression r (Fixed r)
  Divide :: Expression r (Fixed r) -> Expression r (Fixed r) -> Expression r (Fixed r)

  Text :: T.Text -> Expression r T.Text
  Concat :: Expression r T.Text -> Expression r T.Text -> Expression r T.Text
  MathSubstitution :: Expression r (Fixed r) -> Expression r T.Text
  BooleanSubstitution :: Expression r Bool -> Expression r T.Text

instance HasResolution r => Show (Expression r a) where
  show expr =
    case expr of
      Boolean a -> "(Boolean " ++ show a ++ ")"

      Number a -> "(Number " ++ show a ++ ")"
      Add l r -> "(Add " ++ show l ++ " " ++ show r ++ ")"
      Subtract l r -> "(Subtract " ++ show l ++ " " ++ show r ++ ")"
      Multiply l r -> "(Multiply " ++ show l ++ " " ++ show r ++ ")"
      Divide l r -> "(Divide " ++ show l ++ " " ++ show r ++ ")"

      Text a -> "(Text " ++ show a ++ ")"
      Concat a b -> "(Concat " ++ show a ++ " " ++ show b ++ ")"
      MathSubstitution a -> "(MathSubstitution" ++ show a ++ ")"
      BooleanSubstitution a -> "(BooleanSubstitution" ++ show a ++ ")"

instance Eq (Expression r a) where
  (==) l r =
    case (l, r) of
      (Boolean a, Boolean b) -> a == b

      (Number a, Number b) -> a == b
      (Add a b, Add c d) -> (a == c && b == d) || (a == d && b == c)
      (Subtract a b, Subtract c d) -> (a == c && b == d) || (a == d && b == c)
      (Multiply a b, Multiply c d) -> (a == c && b == d) || (a == d && b == c)
      (Divide a b, Divide c d) -> a == c && b == d

      (Text a, Text b) -> a == b
      (Concat a b, Concat c d) -> a == c && b == d
      (MathSubstitution a, MathSubstitution b) -> a == b
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

instance HasResolution r => SubstitutionText (Fixed r) where
  toText = T.dropWhileEnd (`elem` ['.', '0']) . T.pack . show

-- Evaluates an expression
evaluate :: HasResolution r => Expression r a -> Value r
evaluate expr =
  case expr of
    Boolean _ -> BooleanValue $ evaluateBoolean expr

    Number _ -> NumberValue $ evaluateNumber expr
    Add _ _ -> NumberValue $ evaluateNumber expr
    Subtract _ _ -> NumberValue $ evaluateNumber expr
    Multiply _ _ -> NumberValue $ evaluateNumber expr
    Divide _ _ -> NumberValue $ evaluateNumber expr

    Text _ -> TextValue $ evaluateText expr
    Concat _ _ -> TextValue $ evaluateText expr
    MathSubstitution _ -> TextValue $ evaluateText expr
    BooleanSubstitution _ -> TextValue $ evaluateText expr

evaluateBoolean :: Expression r Bool -> Bool
evaluateBoolean expr =
  case expr of
    Boolean a -> a

evaluateNumber :: HasResolution r => Expression r (Fixed r) -> (Fixed r)
evaluateNumber expr =
  case expr of
    Number a -> a
    Add l r -> evaluateNumber l + evaluateNumber r
    Subtract l r -> evaluateNumber l - evaluateNumber r
    Multiply l r -> evaluateNumber l * evaluateNumber r
    Divide l r -> evaluateNumber l / evaluateNumber r

evaluateText :: HasResolution r => Expression r T.Text -> T.Text
evaluateText expr =
  case expr of
    Text a -> a
    Concat a b ->
      evaluateText a <> evaluateText b
    MathSubstitution a ->
      toText $ evaluateNumber a
    BooleanSubstitution a ->
      toText $ evaluateBoolean a

-- Simplifies an expression
simplify :: HasResolution r => Expression r a -> Expression r a
simplify expr =
  case expr of
    Boolean _ -> simplifyBoolean expr

    Number _ -> simplifyMath expr
    Add _ _ -> simplifyMath expr
    Subtract _ _ -> simplifyMath expr
    Multiply _ _ -> simplifyMath expr
    Divide _ _ -> simplifyMath expr

    Text _ -> simplifyText expr
    Concat _ _ -> simplifyText expr
    MathSubstitution _ -> simplifyText expr
    BooleanSubstitution _ -> simplifyText expr

simplifyBoolean :: Expression r Bool -> Expression r Bool
simplifyBoolean expr =
  case expr of
    Boolean _ -> expr

simplifyMath :: HasResolution r => Expression r (Fixed r) -> Expression r (Fixed r)
simplifyMath expr =
  case expr of
    Number a ->
      Number a

    Add (Number l) (Number r) ->
      Number $ l + r
    Add l r ->
      if Add (simplifyMath l) (simplifyMath r) == expr
      then expr
      else simplifyMath $ Add (simplifyMath l) (simplifyMath r)

    Subtract (Number l) (Number r) ->
      Number $ l - r
    Subtract l r ->
      if Subtract (simplifyMath l) (simplifyMath r) == expr
      then expr
      else simplifyMath $ Subtract (simplifyMath l) (simplifyMath r)

    Multiply (Number l) (Number r) ->
      Number $ l * r
    Multiply l r ->
      if Multiply (simplifyMath l) (simplifyMath r) == expr
      then expr
      else simplifyMath $ Multiply (simplifyMath l) (simplifyMath r)

    Divide (Number l) (Number r) ->
      Number $ l / r
    Divide l r ->
      if Divide (simplifyMath l) (simplifyMath r) == expr
      then expr
      else simplifyMath $ Divide (simplifyMath l) (simplifyMath r)

simplifyText :: HasResolution r => Expression r T.Text -> Expression r T.Text
simplifyText expr =
  stripText $ simplifyTextLoop expr

simplifyTextLoop :: HasResolution r => Expression r T.Text -> Expression r T.Text
simplifyTextLoop expr =
  case expr of
    Text a ->
      Text . collapseSpaces $ a

    Concat (Text l) (Text r) ->
      Text . collapseSpaces $ l <> r
    Concat (Text l) (MathSubstitution (Number r)) ->
      Text . collapseSpaces $ l <> toText r
    Concat (Text l) (BooleanSubstitution (Boolean r)) ->
      Text . collapseSpaces $ l <> toText r
    Concat (MathSubstitution (Number l)) (Text r) ->
      Text . collapseSpaces $ toText l <> r
    Concat (MathSubstitution (Number l)) (MathSubstitution (Number r)) ->
      Text . collapseSpaces $ toText l <> toText r
    Concat (MathSubstitution (Number l)) (BooleanSubstitution (Boolean r)) ->
      Text . collapseSpaces $ toText l <> toText r
    Concat (BooleanSubstitution (Boolean l)) (Text r) ->
      Text . collapseSpaces $ toText l <> r
    Concat (BooleanSubstitution (Boolean l)) (MathSubstitution (Number r)) ->
      Text . collapseSpaces $ toText l <> toText r
    Concat (BooleanSubstitution (Boolean l)) (BooleanSubstitution (Boolean r)) ->
      Text . collapseSpaces $ toText l <> toText r
    Concat l r ->
      if Concat (simplifyTextLoop l) (simplifyTextLoop r) == expr
      then expr
      else simplifyTextLoop $ Concat (simplifyTextLoop l) (simplifyTextLoop r)

    MathSubstitution (Number a) ->
      Text . collapseSpaces $ toText a
    MathSubstitution math ->
      if simplifyMath math == math
      then MathSubstitution math
      else simplifyTextLoop $ MathSubstitution (simplifyMath math)
    BooleanSubstitution (Boolean a) ->
      Text . collapseSpaces $ toText a

-- Attempts to trim whitespace from the beginning and end of an expression
stripText :: Expression r T.Text -> Expression r T.Text
stripText expr =
  case expr of
    Text a ->
      Text . T.strip $ a

    Concat (Text l) (Text r) ->
      Concat (Text $ T.stripStart l) (Text $ T.stripEnd r)
    Concat (Text l) r ->
      Concat (Text $ T.stripStart l) r
    Concat l (Text r) ->
      Concat l (Text $ T.stripEnd r)

    _ -> expr

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
