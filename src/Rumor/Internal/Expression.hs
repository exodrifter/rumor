{-# LANGUAGE RankNTypes #-}
module Rumor.Internal.Expression
( Loose(..)
, toBoolean
, toNumber
, toString
, InferenceFailure(..)
, inferenceFailureToText
) where

import Data.Text (Text)
import Data.Scientific (Scientific)
import Rumor.Internal.VariableName (VariableName, variableNameToText)
import Rumor.Internal.VariableType (VariableType(..), typeToText)
import Rumor.Internal.Context (Context, getVariableType)

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
-- >>> let foo = LooseVariable (VariableName (Unicode (NET.new 'f' "oo")))
-- >>> let bar = LooseVariable (VariableName (Unicode (NET.new 'b' "ar")))
-- >>> let string = LooseVariable (VariableName (Unicode (NET.new 's' "tring")))
-- >>> let number = LooseVariable (VariableName (Unicode (NET.new 'n' "umber")))
-- >>> let boolean = LooseVariable (VariableName (Unicode (NET.new 'b' "oolean")))

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

  Can't infer the types of unknown variables on its own.

  >>> toBoolean context foo
  Left (CannotInferVariable (VariableName (Unicode "foo")))

  >>> toBoolean context boolean
  Right (BooleanVariable (VariableName (Unicode "boolean")))

  >>> toBoolean context number
  Left (TypeMismatch (VariableName (Unicode "number")) BooleanType NumberType)

  >>> toBoolean context string
  Left (TypeMismatch (VariableName (Unicode "string")) BooleanType StringType)

  Can infer the types of variables when using logic operators.

  >>> toBoolean context (LooseLogicalNot (BooleanLiteral True))
  Right (LogicalNot (Boolean True))

  >>> toBoolean context (LooseLogicalNot foo)
  Right (LogicalNot (BooleanVariable (VariableName (Unicode "foo"))))

  >>> toBoolean context (LooseLogicalAnd (BooleanLiteral True) bar)
  Right (LogicalAnd (Boolean True) (BooleanVariable (VariableName (Unicode "bar"))))

  >>> toBoolean context (LooseLogicalAnd foo (BooleanLiteral True))
  Right (LogicalAnd (BooleanVariable (VariableName (Unicode "foo"))) (Boolean True))

  >>> toBoolean context (LooseLogicalAnd foo bar)
  Right (LogicalAnd (BooleanVariable (VariableName (Unicode "foo"))) (BooleanVariable (VariableName (Unicode "bar"))))

  >>> toBoolean context (LooseLogicalOr (BooleanLiteral True) bar)
  Right (LogicalOr (Boolean True) (BooleanVariable (VariableName (Unicode "bar"))))

  >>> toBoolean context (LooseLogicalOr foo (BooleanLiteral True))
  Right (LogicalOr (BooleanVariable (VariableName (Unicode "foo"))) (Boolean True))

  >>> toBoolean context (LooseLogicalOr foo bar)
  Right (LogicalOr (BooleanVariable (VariableName (Unicode "foo"))) (BooleanVariable (VariableName (Unicode "bar"))))

  >>> toBoolean context (LooseLogicalXor (BooleanLiteral True) bar)
  Right (LogicalXor (Boolean True) (BooleanVariable (VariableName (Unicode "bar"))))

  >>> toBoolean context (LooseLogicalXor foo (BooleanLiteral True))
  Right (LogicalXor (BooleanVariable (VariableName (Unicode "foo"))) (Boolean True))

  >>> toBoolean context (LooseLogicalXor foo bar)
  Right (LogicalXor (BooleanVariable (VariableName (Unicode "foo"))) (BooleanVariable (VariableName (Unicode "bar"))))

  Can only infer the types of equality operations when the type of at least one
  side is known.

  >>> toBoolean context (LooseEqual foo bar)
  Left CannotInferExpression

  >>> toBoolean context (LooseEqual foo (StringLiteral "bar"))
  Right (Equal (StringVariable (VariableName (Unicode "foo"))) (String "bar"))

  >>> toBoolean context (LooseEqual (StringLiteral "foo") bar)
  Right (Equal (String "foo") (StringVariable (VariableName (Unicode "bar"))))

  >>> toBoolean context (LooseEqual foo (LooseConcat foo bar))
  Right (Equal (StringVariable (VariableName (Unicode "foo"))) (Concat (StringVariable (VariableName (Unicode "foo"))) (StringVariable (VariableName (Unicode "bar")))))

  >>> toBoolean context (LooseNotEqual foo bar)
  Left CannotInferExpression

  >>> toBoolean context (LooseNotEqual foo (StringLiteral "bar"))
  Right (NotEqual (StringVariable (VariableName (Unicode "foo"))) (String "bar"))

  >>> toBoolean context (LooseNotEqual (StringLiteral "foo") bar)
  Right (NotEqual (String "foo") (StringVariable (VariableName (Unicode "bar"))))

  >>> toBoolean context (LooseNotEqual foo (LooseConcat foo bar))
  Right (NotEqual (StringVariable (VariableName (Unicode "foo"))) (Concat (StringVariable (VariableName (Unicode "foo"))) (StringVariable (VariableName (Unicode "bar")))))

  >>> toBoolean context (LooseEqual foo bar)
  Left CannotInferExpression

  >>> toBoolean context (LooseEqual foo (NumberLiteral 1))
  Right (Equal (NumberVariable (VariableName (Unicode "foo"))) (Number 1.0))

  >>> toBoolean context (LooseEqual (NumberLiteral 1) bar)
  Right (Equal (Number 1.0) (NumberVariable (VariableName (Unicode "bar"))))

  >>> toBoolean context (LooseEqual foo (LooseAddition foo bar))
  Right (Equal (NumberVariable (VariableName (Unicode "foo"))) (Addition (NumberVariable (VariableName (Unicode "foo"))) (NumberVariable (VariableName (Unicode "bar")))))

  >>> toBoolean context (LooseNotEqual foo bar)
  Left CannotInferExpression

  >>> toBoolean context (LooseNotEqual foo (NumberLiteral 1))
  Right (NotEqual (NumberVariable (VariableName (Unicode "foo"))) (Number 1.0))

  >>> toBoolean context (LooseNotEqual (NumberLiteral 1) bar)
  Right (NotEqual (Number 1.0) (NumberVariable (VariableName (Unicode "bar"))))

  >>> toBoolean context (LooseNotEqual foo (LooseAddition foo bar))
  Right (NotEqual (NumberVariable (VariableName (Unicode "foo"))) (Addition (NumberVariable (VariableName (Unicode "foo"))) (NumberVariable (VariableName (Unicode "bar")))))

  >>> toBoolean context (LooseEqual foo bar)
  Left CannotInferExpression

  >>> toBoolean context (LooseEqual foo (BooleanLiteral True))
  Right (Equal (BooleanVariable (VariableName (Unicode "foo"))) (Boolean True))

  >>> toBoolean context (LooseEqual (BooleanLiteral True) bar)
  Right (Equal (Boolean True) (BooleanVariable (VariableName (Unicode "bar"))))

  >>> toBoolean context (LooseEqual foo (LooseLogicalAnd foo bar))
  Right (Equal (BooleanVariable (VariableName (Unicode "foo"))) (LogicalAnd (BooleanVariable (VariableName (Unicode "foo"))) (BooleanVariable (VariableName (Unicode "bar")))))

  >>> toBoolean context (LooseNotEqual foo bar)
  Left CannotInferExpression

  >>> toBoolean context (LooseNotEqual foo (BooleanLiteral True))
  Right (NotEqual (BooleanVariable (VariableName (Unicode "foo"))) (Boolean True))

  >>> toBoolean context (LooseNotEqual (BooleanLiteral True) bar)
  Right (NotEqual (Boolean True) (BooleanVariable (VariableName (Unicode "bar"))))

  >>> toBoolean context (LooseNotEqual foo (LooseLogicalAnd foo bar))
  Right (NotEqual (BooleanVariable (VariableName (Unicode "foo"))) (LogicalAnd (BooleanVariable (VariableName (Unicode "foo"))) (BooleanVariable (VariableName (Unicode "bar")))))
-}
toBoolean :: Context -> Loose -> Either InferenceFailure (Expression Bool)
toBoolean context loose =
  let
    overloaded ::
      (forall a. Expression a -> Expression a -> Expression Bool) ->
      Loose ->
      Loose ->
      Either InferenceFailure (Expression Bool)
    overloaded constructor l r =
      case (l, r) of
        (LooseVariable lName, LooseVariable rName) ->
          case (getVariableType lName context, getVariableType rName context) of
            (Just BooleanType, _) ->
              Right (constructor (BooleanVariable lName) (BooleanVariable rName))
            (Just NumberType, _) ->
              Right (constructor (NumberVariable lName) (NumberVariable rName))
            (Just StringType, _) ->
              Right (constructor (StringVariable lName) (StringVariable rName))
            (_, Just BooleanType) ->
              Right (constructor (BooleanVariable lName) (BooleanVariable rName))
            (_, Just NumberType) ->
              Right (constructor (NumberVariable lName) (NumberVariable rName))
            (_, Just StringType) ->
              Right (constructor (StringVariable lName) (StringVariable rName))
            (Nothing, Nothing) ->
              Left CannotInferExpression
        (LooseVariable name, _) ->
          case (go r False, toNumber context r, toString context r) of
            (Right inner, _, _) -> Right (constructor (BooleanVariable name) inner)
            (_, Right inner, _) -> Right (constructor (NumberVariable name) inner)
            (_, _, Right inner) -> Right (constructor (StringVariable name) inner)
            _ -> Left CannotInferExpression
        (_, LooseVariable name) ->
          case (go l False, toNumber context l, toString context l) of
            (Right inner, _, _) -> Right (constructor inner (BooleanVariable name))
            (_, Right inner, _) -> Right (constructor inner (NumberVariable name))
            (_, _, Right inner) -> Right (constructor inner (StringVariable name))
            _ -> Left CannotInferExpression
        (_, _) ->
          case (go l False, toNumber context l, toString context l) of
            (Right inner, _, _) -> constructor inner <$> go r False
            (_, Right inner, _) -> constructor inner <$> toNumber context r
            (_, _, Right inner) -> constructor inner <$> toString context r
            _ -> Left UnexpectedFailure

    go expr first =
      case expr of
        BooleanLiteral b -> Right (Boolean b)
        LooseLogicalNot b -> LogicalNot <$> go b False
        LooseLogicalAnd l r -> LogicalAnd <$> go l False <*> go r False
        LooseLogicalOr l r -> LogicalOr <$> go l False <*> go r False
        LooseLogicalXor l r -> LogicalXor <$> go l False <*> go r False

        NumberLiteral _ -> Left UnexpectedFailure
        LooseAddition _ _ -> Left UnexpectedFailure
        LooseSubtraction _ _ -> Left UnexpectedFailure
        LooseMultiplication _ _ -> Left UnexpectedFailure
        LooseDivision _ _ -> Left UnexpectedFailure

        StringLiteral _ -> Left UnexpectedFailure
        LooseConcat _ _ -> Left UnexpectedFailure

        LooseVariable name -> infer first BooleanVariable BooleanType name context
        LooseEqual l r -> overloaded Equal l r
        LooseNotEqual l r -> overloaded NotEqual l r
        ToString _ -> Left UnexpectedFailure
  in
    go loose True

{-| Converts a loose number expression into a strict number expression, if
  possible. Fails when the type of a variable cannot be inferred.

  Can't infer the types of unknown variables on its own.

  >>> toNumber context foo
  Left (CannotInferVariable (VariableName (Unicode "foo")))

  >>> toNumber context boolean
  Left (TypeMismatch (VariableName (Unicode "boolean")) NumberType BooleanType)

  >>> toNumber context number
  Right (NumberVariable (VariableName (Unicode "number")))

  >>> toNumber context string
  Left (TypeMismatch (VariableName (Unicode "string")) NumberType StringType)

  Can infer the types of variables when using number operators.

  >>> toNumber context (LooseAddition (NumberLiteral 1) bar)
  Right (Addition (Number 1.0) (NumberVariable (VariableName (Unicode "bar"))))

  >>> toNumber context (LooseAddition foo (NumberLiteral 1))
  Right (Addition (NumberVariable (VariableName (Unicode "foo"))) (Number 1.0))

  >>> toNumber context (LooseAddition foo bar)
  Right (Addition (NumberVariable (VariableName (Unicode "foo"))) (NumberVariable (VariableName (Unicode "bar"))))

  >>> toNumber context (LooseSubtraction (NumberLiteral 1) bar)
  Right (Subtraction (Number 1.0) (NumberVariable (VariableName (Unicode "bar"))))

  >>> toNumber context (LooseSubtraction foo (NumberLiteral 1))
  Right (Subtraction (NumberVariable (VariableName (Unicode "foo"))) (Number 1.0))

  >>> toNumber context (LooseSubtraction foo bar)
  Right (Subtraction (NumberVariable (VariableName (Unicode "foo"))) (NumberVariable (VariableName (Unicode "bar"))))

  >>> toNumber context (LooseMultiplication (NumberLiteral 1) bar)
  Right (Multiplication (Number 1.0) (NumberVariable (VariableName (Unicode "bar"))))

  >>> toNumber context (LooseMultiplication foo (NumberLiteral 1))
  Right (Multiplication (NumberVariable (VariableName (Unicode "foo"))) (Number 1.0))

  >>> toNumber context (LooseMultiplication foo bar)
  Right (Multiplication (NumberVariable (VariableName (Unicode "foo"))) (NumberVariable (VariableName (Unicode "bar"))))

  >>> toNumber context (LooseDivision (NumberLiteral 1) bar)
  Right (Division (Number 1.0) (NumberVariable (VariableName (Unicode "bar"))))

  >>> toNumber context (LooseDivision foo (NumberLiteral 1))
  Right (Division (NumberVariable (VariableName (Unicode "foo"))) (Number 1.0))

  >>> toNumber context (LooseDivision foo bar)
  Right (Division (NumberVariable (VariableName (Unicode "foo"))) (NumberVariable (VariableName (Unicode "bar"))))
-}
toNumber :: Context -> Loose -> Either InferenceFailure (Expression Scientific)
toNumber context loose =
  let
    go expr first =
      case expr of
        BooleanLiteral _ -> Left UnexpectedFailure
        LooseLogicalNot _ -> Left UnexpectedFailure
        LooseLogicalAnd _ _ -> Left UnexpectedFailure
        LooseLogicalOr _ _ -> Left UnexpectedFailure
        LooseLogicalXor _ _ -> Left UnexpectedFailure

        NumberLiteral n -> Right (Number n)
        LooseAddition l r -> Addition <$> go l False <*> go r False
        LooseSubtraction l r -> Subtraction <$> go l False <*> go r False
        LooseMultiplication l r -> Multiplication <$> go l False <*> go r False
        LooseDivision l r -> Division <$> go l False <*> go r False

        StringLiteral _ -> Left UnexpectedFailure
        LooseConcat _ _ -> Left UnexpectedFailure

        LooseVariable name -> infer first NumberVariable NumberType name context
        LooseEqual _ _ -> Left UnexpectedFailure
        LooseNotEqual _ _ -> Left UnexpectedFailure
        ToString _ -> Left UnexpectedFailure
  in
    go loose True

{-| Converts a loose string expression into a strict string expression, if
  possible. Fails when the type of a variable cannot be inferred.

  Can't infer the types of unknown variables on its own.

  >>> toString context foo
  Left (CannotInferVariable (VariableName (Unicode "foo")))

  >>> toString context boolean
  Left (TypeMismatch (VariableName (Unicode "boolean")) StringType BooleanType)

  >>> toString context number
  Left (TypeMismatch (VariableName (Unicode "number")) StringType NumberType)

  >>> toString context string
  Right (StringVariable (VariableName (Unicode "string")))

  Can infer the types of variables when using string operators.

  >>> toString context (LooseConcat (StringLiteral ("foo")) bar)
  Right (Concat (String "foo") (StringVariable (VariableName (Unicode "bar"))))

  >>> toString context (LooseConcat foo (StringLiteral ("bar")))
  Right (Concat (StringVariable (VariableName (Unicode "foo"))) (String "bar"))

  >>> toString context (LooseConcat foo bar)
  Right (Concat (StringVariable (VariableName (Unicode "foo"))) (StringVariable (VariableName (Unicode "bar"))))

  Cannot infer the types of variables in ToString expressions, because ToString
  operations are overloaded.

  >>> toString context (ToString foo)
  Left (CannotInferVariable (VariableName (Unicode "foo")))

  >>> toString context (ToString foo)
  Left (CannotInferVariable (VariableName (Unicode "foo")))
-}
toString :: Context -> Loose -> Either InferenceFailure (Expression Text)
toString context loose =
  let
    go expr first =
      case expr of
        BooleanLiteral _ -> Left UnexpectedFailure
        LooseLogicalNot _ -> Left UnexpectedFailure
        LooseLogicalAnd _ _ -> Left UnexpectedFailure
        LooseLogicalOr _ _ -> Left UnexpectedFailure
        LooseLogicalXor _ _ -> Left UnexpectedFailure

        NumberLiteral _ -> Left UnexpectedFailure
        LooseAddition _ _ -> Left UnexpectedFailure
        LooseSubtraction _ _ -> Left UnexpectedFailure
        LooseMultiplication _ _ -> Left UnexpectedFailure
        LooseDivision _ _ -> Left UnexpectedFailure

        StringLiteral s -> Right (String s)
        LooseConcat l r -> Concat <$> go l False <*> go r False

        LooseVariable name -> infer first StringVariable StringType name context
        LooseEqual _ _ -> Left UnexpectedFailure
        LooseNotEqual _ _ -> Left UnexpectedFailure
        ToString inner ->
          case inner of
            BooleanLiteral _ -> BooleanToString <$> toBoolean context inner
            LooseLogicalNot _ -> BooleanToString <$> toBoolean context inner
            LooseLogicalAnd _ _ -> BooleanToString <$> toBoolean context inner
            LooseLogicalOr _ _ -> BooleanToString <$> toBoolean context inner
            LooseLogicalXor _ _ -> BooleanToString <$> toBoolean context inner

            NumberLiteral _ -> NumberToString <$> toNumber context inner
            LooseAddition _ _ -> NumberToString <$> toNumber context inner
            LooseSubtraction _ _ -> NumberToString <$> toNumber context inner
            LooseMultiplication _ _ -> NumberToString <$> toNumber context inner
            LooseDivision _ _ -> NumberToString <$> toNumber context inner

            StringLiteral _ -> go inner False
            LooseConcat _ _ -> go inner False

            LooseVariable name -> Left (CannotInferVariable name)
            LooseEqual _ _ -> BooleanToString <$> toBoolean context inner
            LooseNotEqual _ _ -> BooleanToString <$> toBoolean context inner
            ToString _ -> go inner first -- Nested ToStrings should be treated as one
  in
    go loose True

--------------------------------------------------------------------------------
-- Inference
--------------------------------------------------------------------------------

infer ::
  Bool ->
  (VariableName -> Expression a) ->
  VariableType ->
  VariableName ->
  Context ->
  Either InferenceFailure (Expression a)
infer first constructor expectedType name context =
  case getVariableType name context of
    Just actualType
      | expectedType == actualType -> Right (constructor name)
      | otherwise -> Left (TypeMismatch name expectedType actualType)
    Nothing ->
      if first
      then Left (CannotInferVariable name)
      else Right (constructor name)

-- | The types of inference failures that can occur.
data InferenceFailure =
    CannotInferVariable VariableName
  | CannotInferExpression
  | TypeMismatch VariableName VariableType VariableType
  | UnexpectedFailure
  deriving Show

inferenceFailureToText :: InferenceFailure -> Text
inferenceFailureToText failure =
  case failure of
    CannotInferVariable name ->
      "Cannot infer type of `" <> variableNameToText name <> "`."
    CannotInferExpression ->
      "Cannot infer the type of this expression."
    TypeMismatch name expected actual ->
      "Expected `"
      <> variableNameToText name
      <> "` to be of type "
      <> typeToText expected
      <> ", but it has been defined as or inferred to be a "
      <> typeToText actual
      <> "."
    UnexpectedFailure ->
      "Unexpected error when inferring expression type! \
      \This is a bug; please report this."
