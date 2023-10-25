module Rumor.Internal.Loose
( Loose(..)
, toBoolean
, toNumber
, toString
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
data Loose =
  -- Boolean
    BooleanLiteral Bool
  | LooseLogicalNot Loose
  | LooseLogicalAnd Loose Loose
  | LooseLogicalOr Loose Loose
  | LooseLogicalXor Loose Loose

  -- Number
  | NumberLiteral Scientific
  | LooseAddition Loose Loose
  | LooseSubtraction Loose Loose
  | LooseMultiplication Loose Loose
  | LooseDivision Loose Loose

  -- String
  | StringLiteral Text
  | LooseConcat Loose Loose

  -- Overloaded
  | LooseVariable VariableName
  | LooseEqual Loose Loose
  | LooseNotEqual Loose Loose
  | ToString Loose
  deriving (Eq, Show)

{-| Converts a loose boolean expression into a strict boolean expression, if
  possible. Fails when the type of a variable cannot be inferred.

  Can't infer the types of variables on its own.

  >>> toBoolean foo
  Left (CannotInferVariable (VariableName (Unicode "foo")))

  Can infer the types of variables when using logic operators.

  >>> toBoolean (LooseLogicalNot (BooleanLiteral True))
  Right (LogicalNot (Boolean True))

  >>> toBoolean (LooseLogicalNot foo)
  Right (LogicalNot (BooleanVariable (VariableName (Unicode "foo"))))

  >>> toBoolean (LooseLogicalAnd (BooleanLiteral True) bar)
  Right (LogicalAnd (Boolean True) (BooleanVariable (VariableName (Unicode "bar"))))

  >>> toBoolean (LooseLogicalAnd foo (BooleanLiteral True))
  Right (LogicalAnd (BooleanVariable (VariableName (Unicode "foo"))) (Boolean True))

  >>> toBoolean (LooseLogicalAnd foo bar)
  Right (LogicalAnd (BooleanVariable (VariableName (Unicode "foo"))) (BooleanVariable (VariableName (Unicode "bar"))))

  >>> toBoolean (LooseLogicalOr (BooleanLiteral True) bar)
  Right (LogicalOr (Boolean True) (BooleanVariable (VariableName (Unicode "bar"))))

  >>> toBoolean (LooseLogicalOr foo (BooleanLiteral True))
  Right (LogicalOr (BooleanVariable (VariableName (Unicode "foo"))) (Boolean True))

  >>> toBoolean (LooseLogicalOr foo bar)
  Right (LogicalOr (BooleanVariable (VariableName (Unicode "foo"))) (BooleanVariable (VariableName (Unicode "bar"))))

  >>> toBoolean (LooseLogicalXor (BooleanLiteral True) bar)
  Right (LogicalXor (Boolean True) (BooleanVariable (VariableName (Unicode "bar"))))

  >>> toBoolean (LooseLogicalXor foo (BooleanLiteral True))
  Right (LogicalXor (BooleanVariable (VariableName (Unicode "foo"))) (Boolean True))

  >>> toBoolean (LooseLogicalXor foo bar)
  Right (LogicalXor (BooleanVariable (VariableName (Unicode "foo"))) (BooleanVariable (VariableName (Unicode "bar"))))

  Can only infer the types of equality operations when the type of at least one
  side is known.

  >>> toBoolean (LooseEqual foo bar)
  Left CannotInferExpression

  >>> toBoolean (LooseEqual foo (StringLiteral "bar"))
  Right (EqualString (StringVariable (VariableName (Unicode "foo"))) (String "bar"))

  >>> toBoolean (LooseEqual (StringLiteral "foo") bar)
  Right (EqualString (String "foo") (StringVariable (VariableName (Unicode "bar"))))

  >>> toBoolean (LooseEqual foo (LooseConcat foo bar))
  Right (EqualString (StringVariable (VariableName (Unicode "foo"))) (Concat (StringVariable (VariableName (Unicode "foo"))) (StringVariable (VariableName (Unicode "bar")))))

  >>> toBoolean (LooseNotEqual foo bar)
  Left CannotInferExpression

  >>> toBoolean (LooseNotEqual foo (StringLiteral "bar"))
  Right (NotEqualString (StringVariable (VariableName (Unicode "foo"))) (String "bar"))

  >>> toBoolean (LooseNotEqual (StringLiteral "foo") bar)
  Right (NotEqualString (String "foo") (StringVariable (VariableName (Unicode "bar"))))

  >>> toBoolean (LooseNotEqual foo (LooseConcat foo bar))
  Right (NotEqualString (StringVariable (VariableName (Unicode "foo"))) (Concat (StringVariable (VariableName (Unicode "foo"))) (StringVariable (VariableName (Unicode "bar")))))

  >>> toBoolean (LooseEqual foo bar)
  Left CannotInferExpression

  >>> toBoolean (LooseEqual foo (NumberLiteral 1))
  Right (EqualNumber (NumberVariable (VariableName (Unicode "foo"))) (Number 1.0))

  >>> toBoolean (LooseEqual (NumberLiteral 1) bar)
  Right (EqualNumber (Number 1.0) (NumberVariable (VariableName (Unicode "bar"))))

  >>> toBoolean (LooseEqual foo (LooseAddition foo bar))
  Right (EqualNumber (NumberVariable (VariableName (Unicode "foo"))) (Addition (NumberVariable (VariableName (Unicode "foo"))) (NumberVariable (VariableName (Unicode "bar")))))

  >>> toBoolean (LooseNotEqual foo bar)
  Left CannotInferExpression

  >>> toBoolean (LooseNotEqual foo (NumberLiteral 1))
  Right (NotEqualNumber (NumberVariable (VariableName (Unicode "foo"))) (Number 1.0))

  >>> toBoolean (LooseNotEqual (NumberLiteral 1) bar)
  Right (NotEqualNumber (Number 1.0) (NumberVariable (VariableName (Unicode "bar"))))

  >>> toBoolean (LooseNotEqual foo (LooseAddition foo bar))
  Right (NotEqualNumber (NumberVariable (VariableName (Unicode "foo"))) (Addition (NumberVariable (VariableName (Unicode "foo"))) (NumberVariable (VariableName (Unicode "bar")))))

  >>> toBoolean (LooseEqual foo bar)
  Left CannotInferExpression

  >>> toBoolean (LooseEqual foo (BooleanLiteral True))
  Right (EqualBoolean (BooleanVariable (VariableName (Unicode "foo"))) (Boolean True))

  >>> toBoolean (LooseEqual (BooleanLiteral True) bar)
  Right (EqualBoolean (Boolean True) (BooleanVariable (VariableName (Unicode "bar"))))

  >>> toBoolean (LooseEqual foo (LooseLogicalAnd foo bar))
  Right (EqualBoolean (BooleanVariable (VariableName (Unicode "foo"))) (LogicalAnd (BooleanVariable (VariableName (Unicode "foo"))) (BooleanVariable (VariableName (Unicode "bar")))))

  >>> toBoolean (LooseNotEqual foo bar)
  Left CannotInferExpression

  >>> toBoolean (LooseNotEqual foo (BooleanLiteral True))
  Right (NotEqualBoolean (BooleanVariable (VariableName (Unicode "foo"))) (Boolean True))

  >>> toBoolean (LooseNotEqual (BooleanLiteral True) bar)
  Right (NotEqualBoolean (Boolean True) (BooleanVariable (VariableName (Unicode "bar"))))

  >>> toBoolean (LooseNotEqual foo (LooseLogicalAnd foo bar))
  Right (NotEqualBoolean (BooleanVariable (VariableName (Unicode "foo"))) (LogicalAnd (BooleanVariable (VariableName (Unicode "foo"))) (BooleanVariable (VariableName (Unicode "bar")))))
-}
toBoolean :: Loose -> Either InferenceFailure (Expression Bool)
toBoolean loose =
  let
    go expr first =
      case expr of
        BooleanLiteral b -> Right (Boolean b)
        LooseLogicalNot b -> LogicalNot <$> go b False
        LooseLogicalAnd l r -> LogicalAnd <$> go l False <*> go r False
        LooseLogicalOr l r -> LogicalOr <$> go l False <*> go r False
        LooseLogicalXor l r -> LogicalXor <$> go l False <*> go r False

        NumberLiteral _ -> Left TypeMismatch
        LooseAddition _ _ -> Left TypeMismatch
        LooseSubtraction _ _ -> Left TypeMismatch
        LooseMultiplication _ _ -> Left TypeMismatch
        LooseDivision _ _ -> Left TypeMismatch

        StringLiteral _ -> Left TypeMismatch
        LooseConcat _ _ -> Left TypeMismatch

        LooseVariable name ->
          if first
          then Left (CannotInferVariable name)
          else Right (BooleanVariable name)
        LooseEqual l r ->
          case (l, r) of
            (LooseVariable _, LooseVariable _) -> Left CannotInferExpression
            (LooseVariable name, _) ->
              case (go r False, toNumber r, toString r) of
                (Right inner, _, _) -> Right (EqualBoolean (BooleanVariable name) inner)
                (_, Right inner, _) -> Right (EqualNumber (NumberVariable name) inner)
                (_, _, Right inner) -> Right (EqualString (StringVariable name) inner)
                _ -> Left CannotInferExpression
            (_, LooseVariable name) ->
              case (go l False, toNumber l, toString l) of
                (Right inner, _, _) -> Right (EqualBoolean inner (BooleanVariable name))
                (_, Right inner, _) -> Right (EqualNumber inner (NumberVariable name))
                (_, _, Right inner) -> Right (EqualString inner (StringVariable name))
                _ -> Left CannotInferExpression
            (_, _) ->
              case (go l False, toNumber l, toString l) of
                (Right inner, _, _) -> EqualBoolean inner <$> go r False
                (_, Right inner, _) -> EqualNumber inner <$> toNumber r
                (_, _, Right inner) -> EqualString inner <$> toString r
                _ -> Left TypeMismatch
        LooseNotEqual l r ->
          case (l, r) of
            (LooseVariable _, LooseVariable _) -> Left CannotInferExpression
            (LooseVariable name, _) ->
              case (go r False, toNumber r, toString r) of
                (Right inner, _, _) -> Right (NotEqualBoolean (BooleanVariable name) inner)
                (_, Right inner, _) -> Right (NotEqualNumber (NumberVariable name) inner)
                (_, _, Right inner) -> Right (NotEqualString (StringVariable name) inner)
                _ -> Left CannotInferExpression
            (_, LooseVariable name) ->
              case (go l False, toNumber l, toString l) of
                (Right inner, _, _) -> Right (NotEqualBoolean inner (BooleanVariable name))
                (_, Right inner, _) -> Right (NotEqualNumber inner (NumberVariable name))
                (_, _, Right inner) -> Right (NotEqualString inner (StringVariable name))
                _ -> Left CannotInferExpression
            (_, _) ->
              case (go l False, toNumber l, toString l) of
                (Right inner, _, _) -> NotEqualBoolean inner <$> go r False
                (_, Right inner, _) -> NotEqualNumber inner <$> toNumber r
                (_, _, Right inner) -> NotEqualString inner <$> toString r
                _ -> Left TypeMismatch
        ToString _ -> Left TypeMismatch
  in
    go loose True

{-| Converts a loose number expression into a strict number expression, if
  possible. Fails when the type of a variable cannot be inferred.

  Can infer the types of variables when using number operators.

  >>> toNumber (LooseAddition (NumberLiteral 1) bar)
  Right (Addition (Number 1.0) (NumberVariable (VariableName (Unicode "bar"))))

  >>> toNumber (LooseAddition foo (NumberLiteral 1))
  Right (Addition (NumberVariable (VariableName (Unicode "foo"))) (Number 1.0))

  >>> toNumber (LooseAddition foo bar)
  Right (Addition (NumberVariable (VariableName (Unicode "foo"))) (NumberVariable (VariableName (Unicode "bar"))))

  >>> toNumber (LooseSubtraction (NumberLiteral 1) bar)
  Right (Subtraction (Number 1.0) (NumberVariable (VariableName (Unicode "bar"))))

  >>> toNumber (LooseSubtraction foo (NumberLiteral 1))
  Right (Subtraction (NumberVariable (VariableName (Unicode "foo"))) (Number 1.0))

  >>> toNumber (LooseSubtraction foo bar)
  Right (Subtraction (NumberVariable (VariableName (Unicode "foo"))) (NumberVariable (VariableName (Unicode "bar"))))

  >>> toNumber (LooseMultiplication (NumberLiteral 1) bar)
  Right (Multiplication (Number 1.0) (NumberVariable (VariableName (Unicode "bar"))))

  >>> toNumber (LooseMultiplication foo (NumberLiteral 1))
  Right (Multiplication (NumberVariable (VariableName (Unicode "foo"))) (Number 1.0))

  >>> toNumber (LooseMultiplication foo bar)
  Right (Multiplication (NumberVariable (VariableName (Unicode "foo"))) (NumberVariable (VariableName (Unicode "bar"))))

  >>> toNumber (LooseDivision (NumberLiteral 1) bar)
  Right (Division (Number 1.0) (NumberVariable (VariableName (Unicode "bar"))))

  >>> toNumber (LooseDivision foo (NumberLiteral 1))
  Right (Division (NumberVariable (VariableName (Unicode "foo"))) (Number 1.0))

  >>> toNumber (LooseDivision foo bar)
  Right (Division (NumberVariable (VariableName (Unicode "foo"))) (NumberVariable (VariableName (Unicode "bar"))))
-}
toNumber :: Loose -> Either InferenceFailure (Expression Scientific)
toNumber loose =
  let
    go expr first =
      case expr of
        BooleanLiteral _ -> Left TypeMismatch
        LooseLogicalNot _ -> Left TypeMismatch
        LooseLogicalAnd _ _ -> Left TypeMismatch
        LooseLogicalOr _ _ -> Left TypeMismatch
        LooseLogicalXor _ _ -> Left TypeMismatch

        NumberLiteral n -> Right (Number n)
        LooseAddition l r -> Addition <$> go l False <*> go r False
        LooseSubtraction l r -> Subtraction <$> go l False <*> go r False
        LooseMultiplication l r -> Multiplication <$> go l False <*> go r False
        LooseDivision l r -> Division <$> go l False <*> go r False

        StringLiteral _ -> Left TypeMismatch
        LooseConcat _ _ -> Left TypeMismatch

        LooseVariable name ->
          if first
          then Left (CannotInferVariable name)
          else Right (NumberVariable name)
        LooseEqual _ _ -> Left TypeMismatch
        LooseNotEqual _ _ -> Left TypeMismatch
        ToString _ -> Left TypeMismatch
  in
    go loose True

{-| Converts a loose string expression into a strict string expression, if
  possible. Fails when the type of a variable cannot be inferred.

  Can infer the types of variables when using string operators.

  >>> toString (LooseConcat (StringLiteral ("foo")) bar)
  Right (Concat (String "foo") (StringVariable (VariableName (Unicode "bar"))))

  >>> toString (LooseConcat foo (StringLiteral ("bar")))
  Right (Concat (StringVariable (VariableName (Unicode "foo"))) (String "bar"))

  >>> toString (LooseConcat foo bar)
  Right (Concat (StringVariable (VariableName (Unicode "foo"))) (StringVariable (VariableName (Unicode "bar"))))

  Cannot infer the types of variables in ToString expressions, because ToString
  operations are overloaded.

  >>> toString (ToString foo)
  Left (CannotInferVariable (VariableName (Unicode "foo")))

  >>> toString (ToString foo)
  Left (CannotInferVariable (VariableName (Unicode "foo")))
-}
toString :: Loose -> Either InferenceFailure (Expression Text)
toString loose =
  let
    go expr first =
      case expr of
        BooleanLiteral _ -> Left TypeMismatch
        LooseLogicalNot _ -> Left TypeMismatch
        LooseLogicalAnd _ _ -> Left TypeMismatch
        LooseLogicalOr _ _ -> Left TypeMismatch
        LooseLogicalXor _ _ -> Left TypeMismatch

        NumberLiteral _ -> Left TypeMismatch
        LooseAddition _ _ -> Left TypeMismatch
        LooseSubtraction _ _ -> Left TypeMismatch
        LooseMultiplication _ _ -> Left TypeMismatch
        LooseDivision _ _ -> Left TypeMismatch

        StringLiteral s -> Right (String s)
        LooseConcat l r -> Concat <$> go l False <*> go r False

        LooseVariable name -> Right (StringVariable name)
        LooseEqual _ _ -> Left TypeMismatch
        LooseNotEqual _ _ -> Left TypeMismatch
        ToString inner ->
          case inner of
            BooleanLiteral _ -> BooleanToString <$> toBoolean inner
            LooseLogicalNot _ -> BooleanToString <$> toBoolean inner
            LooseLogicalAnd _ _ -> BooleanToString <$> toBoolean inner
            LooseLogicalOr _ _ -> BooleanToString <$> toBoolean inner
            LooseLogicalXor _ _ -> BooleanToString <$> toBoolean inner

            NumberLiteral _ -> NumberToString <$> toNumber inner
            LooseAddition _ _ -> NumberToString <$> toNumber inner
            LooseSubtraction _ _ -> NumberToString <$> toNumber inner
            LooseMultiplication _ _ -> NumberToString <$> toNumber inner
            LooseDivision _ _ -> NumberToString <$> toNumber inner

            StringLiteral _ -> go inner False
            LooseConcat _ _ -> go inner False

            LooseVariable name -> Left (CannotInferVariable name)
            LooseEqual _ _ -> BooleanToString <$> toBoolean inner
            LooseNotEqual _ _ -> BooleanToString <$> toBoolean inner
            ToString _ -> go inner first -- Nested ToStrings should be treated as one
  in
    go loose True

-- | The types of inference failures that can occur.
data InferenceFailure =
    CannotInferVariable VariableName
  | CannotInferExpression
  | TypeMismatch
  deriving Show

inferenceFailureToText :: InferenceFailure -> Text
inferenceFailureToText failure =
  case failure of
    CannotInferVariable name ->
      "Cannot infer type of `" <> variableNameToText name <> "`."
    CannotInferExpression ->
      "Cannot infer the type of this expression."
    TypeMismatch ->
      "Unexpected error when inferring expression type! \
      \This is a bug; please report this."
