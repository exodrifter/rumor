{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}

module Rumor.Internal.Loose
( Loose(..)
, looseToExpression
, InferenceFailure(..)
, inferenceFailureToText
) where

import Data.Text (Text)
import Data.Scientific (Scientific)
import Rumor.Internal.Expression (Expression(..))
import Rumor.Internal.VariableName (VariableName, variableNameToText)

-- $setup
-- >>> import qualified Data.NonEmptyText as NET
-- >>> import Rumor.Internal.VariableName
-- >>> import Rumor.Internal.Unicode
-- >>> let foo = LooseVariable (VariableName (Unicode (NET.new 'f' "oo")))
-- >>> let bar = LooseVariable (VariableName (Unicode (NET.new 'b' "ar")))

{-| When parsing, Rumor will parse expressions into this type, which is an
  expression which is loosely typed -- the types of the variables are not known.

  This is lets us do the parsing step separately from the type inference step.
-}
data Loose a where
  LooseVariable :: VariableName -> Loose a

  StringLiteral :: Text -> Loose Text
  LooseConcat :: Loose Text -> Loose Text -> Loose Text

  NumberLiteral :: Scientific -> Loose Scientific
  LooseAddition :: Loose Scientific -> Loose Scientific -> Loose Scientific
  LooseSubtraction :: Loose Scientific -> Loose Scientific -> Loose Scientific
  LooseMultiplication :: Loose Scientific -> Loose Scientific -> Loose Scientific
  LooseDivision :: Loose Scientific -> Loose Scientific -> Loose Scientific
  LooseNumberToString :: Loose Scientific -> Loose Text

  BooleanLiteral :: Bool -> Loose Bool
  LooseLogicalNot :: Loose Bool -> Loose Bool
  LooseLogicalAnd :: Loose Bool -> Loose Bool -> Loose Bool
  LooseLogicalOr :: Loose Bool -> Loose Bool -> Loose Bool
  LooseLogicalXor :: Loose Bool -> Loose Bool -> Loose Bool
  LooseBooleanToString :: Loose Bool -> Loose Text

  LooseEqualString :: Loose Text -> Loose Text -> Loose Bool
  LooseNotEqualString :: Loose Text -> Loose Text -> Loose Bool
  LooseEqualNumber :: Loose Scientific -> Loose Scientific -> Loose Bool
  LooseNotEqualNumber :: Loose Scientific -> Loose Scientific -> Loose Bool
  LooseEqualBoolean :: Loose Bool -> Loose Bool -> Loose Bool
  LooseNotEqualBoolean :: Loose Bool -> Loose Bool -> Loose Bool

deriving instance Eq (Loose a)
deriving instance Show (Loose a)

{-| Converts a loose expression into a strict expression, if possible. Fails
  when the type of a variable cannot be inferred.

  Can't infer the types of variables on its own.

  >>> looseToExpression foo
  Left (CannotInferVariable (VariableName (Unicode "foo")))

  Can infer the types of variables when using string operators.

  >>> looseToExpression (LooseConcat (StringLiteral ("foo")) bar)
  Right (Concat (String "foo") (StringVariable (VariableName (Unicode "bar"))))

  >>> looseToExpression (LooseConcat foo (StringLiteral ("bar")))
  Right (Concat (StringVariable (VariableName (Unicode "foo"))) (String "bar"))

  >>> looseToExpression (LooseConcat foo bar)
  Right (Concat (StringVariable (VariableName (Unicode "foo"))) (StringVariable (VariableName (Unicode "bar"))))

  Can infer the types of variables when using number operators.

  >>> looseToExpression (LooseAddition (NumberLiteral 1) bar)
  Right (Addition (Number 1.0) (NumberVariable (VariableName (Unicode "bar"))))

  >>> looseToExpression (LooseAddition foo (NumberLiteral 1))
  Right (Addition (NumberVariable (VariableName (Unicode "foo"))) (Number 1.0))

  >>> looseToExpression (LooseAddition foo bar)
  Right (Addition (NumberVariable (VariableName (Unicode "foo"))) (NumberVariable (VariableName (Unicode "bar"))))

  >>> looseToExpression (LooseSubtraction (NumberLiteral 1) bar)
  Right (Subtraction (Number 1.0) (NumberVariable (VariableName (Unicode "bar"))))

  >>> looseToExpression (LooseSubtraction foo (NumberLiteral 1))
  Right (Subtraction (NumberVariable (VariableName (Unicode "foo"))) (Number 1.0))

  >>> looseToExpression (LooseSubtraction foo bar)
  Right (Subtraction (NumberVariable (VariableName (Unicode "foo"))) (NumberVariable (VariableName (Unicode "bar"))))

  >>> looseToExpression (LooseMultiplication (NumberLiteral 1) bar)
  Right (Multiplication (Number 1.0) (NumberVariable (VariableName (Unicode "bar"))))

  >>> looseToExpression (LooseMultiplication foo (NumberLiteral 1))
  Right (Multiplication (NumberVariable (VariableName (Unicode "foo"))) (Number 1.0))

  >>> looseToExpression (LooseMultiplication foo bar)
  Right (Multiplication (NumberVariable (VariableName (Unicode "foo"))) (NumberVariable (VariableName (Unicode "bar"))))

  >>> looseToExpression (LooseDivision (NumberLiteral 1) bar)
  Right (Division (Number 1.0) (NumberVariable (VariableName (Unicode "bar"))))

  >>> looseToExpression (LooseDivision foo (NumberLiteral 1))
  Right (Division (NumberVariable (VariableName (Unicode "foo"))) (Number 1.0))

  >>> looseToExpression (LooseDivision foo bar)
  Right (Division (NumberVariable (VariableName (Unicode "foo"))) (NumberVariable (VariableName (Unicode "bar"))))

  Can infer the types of variables when using logic operators.

  >>> looseToExpression (LooseLogicalNot (BooleanLiteral True))
  Right (LogicalNot (Boolean True))

  >>> looseToExpression (LooseLogicalNot foo)
  Right (LogicalNot (BooleanVariable (VariableName (Unicode "foo"))))

  >>> looseToExpression (LooseLogicalAnd (BooleanLiteral True) bar)
  Right (LogicalAnd (Boolean True) (BooleanVariable (VariableName (Unicode "bar"))))

  >>> looseToExpression (LooseLogicalAnd foo (BooleanLiteral True))
  Right (LogicalAnd (BooleanVariable (VariableName (Unicode "foo"))) (Boolean True))

  >>> looseToExpression (LooseLogicalAnd foo bar)
  Right (LogicalAnd (BooleanVariable (VariableName (Unicode "foo"))) (BooleanVariable (VariableName (Unicode "bar"))))

  >>> looseToExpression (LooseLogicalOr (BooleanLiteral True) bar)
  Right (LogicalOr (Boolean True) (BooleanVariable (VariableName (Unicode "bar"))))

  >>> looseToExpression (LooseLogicalOr foo (BooleanLiteral True))
  Right (LogicalOr (BooleanVariable (VariableName (Unicode "foo"))) (Boolean True))

  >>> looseToExpression (LooseLogicalOr foo bar)
  Right (LogicalOr (BooleanVariable (VariableName (Unicode "foo"))) (BooleanVariable (VariableName (Unicode "bar"))))

  >>> looseToExpression (LooseLogicalXor (BooleanLiteral True) bar)
  Right (LogicalXor (Boolean True) (BooleanVariable (VariableName (Unicode "bar"))))

  >>> looseToExpression (LooseLogicalXor foo (BooleanLiteral True))
  Right (LogicalXor (BooleanVariable (VariableName (Unicode "foo"))) (Boolean True))

  >>> looseToExpression (LooseLogicalXor foo bar)
  Right (LogicalXor (BooleanVariable (VariableName (Unicode "foo"))) (BooleanVariable (VariableName (Unicode "bar"))))

  Cannot infer the types of variables in ToString expressions, because ToString
  operations are overloaded.

  >>> looseToExpression (LooseNumberToString foo)
  Left (CannotInferVariable (VariableName (Unicode "foo")))

  >>> looseToExpression (LooseBooleanToString foo)
  Left (CannotInferVariable (VariableName (Unicode "foo")))

  Can only infer the types of equality operations when the type of at least one
  side is known.

  >>> looseToExpression (LooseEqualString foo bar)
  Left CannotInferExpression

  >>> looseToExpression (LooseEqualString foo (StringLiteral "bar"))
  Right (EqualString (StringVariable (VariableName (Unicode "foo"))) (String "bar"))

  >>> looseToExpression (LooseEqualString (StringLiteral "foo") bar)
  Right (EqualString (String "foo") (StringVariable (VariableName (Unicode "bar"))))

  >>> looseToExpression (LooseEqualString foo (LooseConcat foo bar))
  Right (EqualString (StringVariable (VariableName (Unicode "foo"))) (Concat (StringVariable (VariableName (Unicode "foo"))) (StringVariable (VariableName (Unicode "bar")))))

  >>> looseToExpression (LooseNotEqualString foo bar)
  Left CannotInferExpression

  >>> looseToExpression (LooseNotEqualString foo (StringLiteral "bar"))
  Right (NotEqualString (StringVariable (VariableName (Unicode "foo"))) (String "bar"))

  >>> looseToExpression (LooseNotEqualString (StringLiteral "foo") bar)
  Right (NotEqualString (String "foo") (StringVariable (VariableName (Unicode "bar"))))

  >>> looseToExpression (LooseNotEqualString foo (LooseConcat foo bar))
  Right (NotEqualString (StringVariable (VariableName (Unicode "foo"))) (Concat (StringVariable (VariableName (Unicode "foo"))) (StringVariable (VariableName (Unicode "bar")))))

  >>> looseToExpression (LooseEqualNumber foo bar)
  Left CannotInferExpression

  >>> looseToExpression (LooseEqualNumber foo (NumberLiteral 1))
  Right (EqualNumber (NumberVariable (VariableName (Unicode "foo"))) (Number 1.0))

  >>> looseToExpression (LooseEqualNumber (NumberLiteral 1) bar)
  Right (EqualNumber (Number 1.0) (NumberVariable (VariableName (Unicode "bar"))))

  >>> looseToExpression (LooseEqualNumber foo (LooseAddition foo bar))
  Right (EqualNumber (NumberVariable (VariableName (Unicode "foo"))) (Addition (NumberVariable (VariableName (Unicode "foo"))) (NumberVariable (VariableName (Unicode "bar")))))

  >>> looseToExpression (LooseNotEqualNumber foo bar)
  Left CannotInferExpression

  >>> looseToExpression (LooseNotEqualNumber foo (NumberLiteral 1))
  Right (NotEqualNumber (NumberVariable (VariableName (Unicode "foo"))) (Number 1.0))

  >>> looseToExpression (LooseNotEqualNumber (NumberLiteral 1) bar)
  Right (NotEqualNumber (Number 1.0) (NumberVariable (VariableName (Unicode "bar"))))

  >>> looseToExpression (LooseNotEqualNumber foo (LooseAddition foo bar))
  Right (NotEqualNumber (NumberVariable (VariableName (Unicode "foo"))) (Addition (NumberVariable (VariableName (Unicode "foo"))) (NumberVariable (VariableName (Unicode "bar")))))

  >>> looseToExpression (LooseEqualBoolean foo bar)
  Left CannotInferExpression

  >>> looseToExpression (LooseEqualBoolean foo (BooleanLiteral True))
  Right (EqualBoolean (BooleanVariable (VariableName (Unicode "foo"))) (Boolean True))

  >>> looseToExpression (LooseEqualBoolean (BooleanLiteral True) bar)
  Right (EqualBoolean (Boolean True) (BooleanVariable (VariableName (Unicode "bar"))))

  >>> looseToExpression (LooseEqualBoolean foo (LooseLogicalAnd foo bar))
  Right (EqualBoolean (BooleanVariable (VariableName (Unicode "foo"))) (LogicalAnd (BooleanVariable (VariableName (Unicode "foo"))) (BooleanVariable (VariableName (Unicode "bar")))))

  >>> looseToExpression (LooseNotEqualBoolean foo bar)
  Left CannotInferExpression

  >>> looseToExpression (LooseNotEqualBoolean foo (BooleanLiteral True))
  Right (NotEqualBoolean (BooleanVariable (VariableName (Unicode "foo"))) (Boolean True))

  >>> looseToExpression (LooseNotEqualBoolean (BooleanLiteral True) bar)
  Right (NotEqualBoolean (Boolean True) (BooleanVariable (VariableName (Unicode "bar"))))

  >>> looseToExpression (LooseNotEqualBoolean foo (LooseLogicalAnd foo bar))
  Right (NotEqualBoolean (BooleanVariable (VariableName (Unicode "foo"))) (LogicalAnd (BooleanVariable (VariableName (Unicode "foo"))) (BooleanVariable (VariableName (Unicode "bar")))))
-}
looseToExpression :: Loose a -> Either InferenceFailure (Expression a)
looseToExpression loose =
  let
    stringOperator ::
      Loose Text ->
      Loose Text ->
      (Expression Text -> Expression Text -> Expression Text) ->
      Either InferenceFailure (Expression Text)
    stringOperator l r op =
      case (l, r) of
        (LooseVariable leftName, LooseVariable rightName) ->
          Right (op (StringVariable leftName) (StringVariable rightName))
        (LooseVariable name, _) ->
          op (StringVariable name) <$> looseToExpression r
        (_, LooseVariable name) ->
          op <$> looseToExpression l
             <*> pure (StringVariable name)
        (_, _) ->
          op <$> looseToExpression l
             <*> looseToExpression r

    numberOperator ::
      Loose Scientific ->
      Loose Scientific ->
      (Expression Scientific -> Expression Scientific -> Expression Scientific) ->
      Either InferenceFailure (Expression Scientific)
    numberOperator l r op =
      case (l, r) of
        (LooseVariable leftName, LooseVariable rightName) ->
          Right (op (NumberVariable leftName) (NumberVariable rightName))
        (LooseVariable name, _) ->
          op (NumberVariable name) <$> looseToExpression r
        (_, LooseVariable name) ->
          op <$> looseToExpression l
             <*> pure (NumberVariable name)
        (_, _) ->
          op <$> looseToExpression l
             <*> looseToExpression r

    booleanOperator ::
      Loose Bool ->
      Loose Bool ->
      (Expression Bool -> Expression Bool -> Expression Bool) ->
      Either InferenceFailure (Expression Bool)
    booleanOperator l r op =
      case (l, r) of
        (LooseVariable leftName, LooseVariable rightName) ->
          Right (op (BooleanVariable leftName) (BooleanVariable rightName))
        (LooseVariable name, _) ->
          op (BooleanVariable name) <$> looseToExpression r
        (_, LooseVariable name) ->
          op <$> looseToExpression l
             <*> pure (BooleanVariable name)
        (_, _) ->
          op <$> looseToExpression l
             <*> looseToExpression r

    booleanUnaryOperator ::
      Loose Bool ->
      (Expression Bool -> Expression Bool) ->
      Either InferenceFailure (Expression Bool)
    booleanUnaryOperator inner op =
        case inner of
          (LooseVariable name) -> Right (op (BooleanVariable name))
          _ -> op <$> looseToExpression inner

    overloadedStringOperator ::
      Loose Text ->
      Loose Text ->
      (Expression Text -> Expression Text -> Expression Bool) ->
      Either InferenceFailure (Expression Bool)
    overloadedStringOperator l r op = do
      case (looseToExpression l, looseToExpression r) of
        (Left _, Right rightExpression) ->
          Right (op (infer l StringVariable) rightExpression)
        (Right leftExpression, Left _) ->
          Right (op leftExpression (infer r StringVariable))
        (Right leftExpression, Right rightExpression) ->
          Right (op leftExpression rightExpression)
        (Left _, Left _) ->
          Left CannotInferExpression

    overloadedNumberOperator ::
      Loose Scientific ->
      Loose Scientific ->
      (Expression Scientific -> Expression Scientific -> Expression Bool) ->
      Either InferenceFailure (Expression Bool)
    overloadedNumberOperator l r op = do
      case (looseToExpression l, looseToExpression r) of
        (Left _, Right rightExpression) ->
          Right (op (infer l NumberVariable) rightExpression)
        (Right leftExpression, Left _) ->
          Right (op leftExpression (infer r NumberVariable))
        (Right leftExpression, Right rightExpression) ->
          Right (op leftExpression rightExpression)
        (Left _, Left _) ->
          Left CannotInferExpression

    overloadedBooleanOperator ::
      Loose Bool ->
      Loose Bool ->
      (Expression Bool -> Expression Bool -> Expression Bool) ->
      Either InferenceFailure (Expression Bool)
    overloadedBooleanOperator l r op = do
      case (looseToExpression l, looseToExpression r) of
        (Left _, Right rightExpression) ->
          Right (op (infer l BooleanVariable) rightExpression)
        (Right leftExpression, Left _) ->
          Right (op leftExpression (infer r BooleanVariable))
        (Right leftExpression, Right rightExpression) ->
          Right (op leftExpression rightExpression)
        (Left _, Left _) ->
          Left CannotInferExpression
  in
    case loose of
      LooseVariable variableName ->
        Left (CannotInferVariable variableName)

      StringLiteral text -> Right (String text)
      LooseConcat l r -> stringOperator l r Concat

      NumberLiteral number -> Right (Number number)
      LooseAddition l r -> numberOperator l r Addition
      LooseSubtraction l r -> numberOperator l r Subtraction
      LooseMultiplication l r -> numberOperator l r Multiplication
      LooseDivision l r -> numberOperator l r Division
      LooseNumberToString inner ->
        NumberToString <$> looseToExpression inner

      BooleanLiteral boolean -> Right (Boolean boolean)
      LooseLogicalNot inner -> booleanUnaryOperator inner LogicalNot
      LooseLogicalAnd l r -> booleanOperator l r LogicalAnd
      LooseLogicalOr l r -> booleanOperator l r LogicalOr
      LooseLogicalXor l r -> booleanOperator l r LogicalXor
      LooseBooleanToString inner -> BooleanToString <$> looseToExpression inner

      LooseEqualString l r -> overloadedStringOperator l r EqualString
      LooseNotEqualString l r -> overloadedStringOperator l r NotEqualString
      LooseEqualNumber l r -> overloadedNumberOperator l r EqualNumber
      LooseNotEqualNumber l r -> overloadedNumberOperator l r NotEqualNumber
      LooseEqualBoolean l r -> overloadedBooleanOperator l r EqualBoolean
      LooseNotEqualBoolean l r -> overloadedBooleanOperator l r NotEqualBoolean

{-| Converts a loose expression into a strictly typed one by treating all
  variables as the expected type.
-}
infer :: Loose a -> (VariableName -> Expression a) -> Expression a
infer loose var =
  case loose of
    LooseVariable variableName ->
      var variableName

    StringLiteral text -> String text
    LooseConcat l r -> Concat (infer l var) (infer r var)

    NumberLiteral number -> Number number
    LooseAddition l r -> Addition (infer l var) (infer r var)
    LooseSubtraction l r -> Subtraction (infer l var) (infer r var)
    LooseMultiplication l r -> Multiplication (infer l var) (infer r var)
    LooseDivision l r -> Division (infer l var) (infer r var)
    LooseNumberToString inner -> NumberToString (infer inner NumberVariable)

    BooleanLiteral boolean -> Boolean boolean
    LooseLogicalNot inner -> LogicalNot (infer inner var)
    LooseLogicalAnd l r -> LogicalAnd (infer l var) (infer r var)
    LooseLogicalOr l r -> LogicalOr (infer l var) (infer r var)
    LooseLogicalXor l r -> LogicalXor (infer l var) (infer r var)
    LooseBooleanToString inner -> BooleanToString (infer inner BooleanVariable)

    LooseEqualString l r -> EqualString (infer l StringVariable) (infer r StringVariable)
    LooseNotEqualString l r -> NotEqualString (infer l StringVariable) (infer r StringVariable)
    LooseEqualNumber l r -> EqualNumber (infer l NumberVariable) (infer r NumberVariable)
    LooseNotEqualNumber l r -> NotEqualNumber (infer l NumberVariable) (infer r NumberVariable)
    LooseEqualBoolean l r -> EqualBoolean (infer l BooleanVariable) (infer r BooleanVariable)
    LooseNotEqualBoolean l r -> NotEqualBoolean (infer l BooleanVariable) (infer r BooleanVariable)

-- | The types of inference failures that can occur.
data InferenceFailure =
    CannotInferVariable VariableName
  | CannotInferExpression
  deriving Show

inferenceFailureToText :: InferenceFailure -> Text
inferenceFailureToText failure =
  case failure of
    CannotInferVariable name ->
      "Cannot infer type of `" <> variableNameToText name <> "`."
    CannotInferExpression ->
      "Cannot infer the type of this expression."
