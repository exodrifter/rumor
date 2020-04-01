module Rumor.Expression.EvaluateTest
( tests
) where

import Rumor.Expression.Type (Expression(..))
import Prelude (Bool(..), ($))
import Test.HUnit
import qualified Rumor.Expression.Type as Expression

tests :: Test
tests =
  TestList
    -- Boolean tests
    [ evaluateSimpleBooleanTest

    -- Number tests
    , evaluateSimpleNumberTest

    -- Text tests
    , evaluateSimpleTextTest
    ]

evaluateSimpleBooleanTest :: Test
evaluateSimpleBooleanTest =
  TestCase $ assertEqual "evaluateBool returns correct boolean"
    True (Expression.evaluateBoolean $ Boolean True)

evaluateSimpleNumberTest :: Test
evaluateSimpleNumberTest =
  TestCase $ assertEqual "evaluateNumber returns correct number"
    3 (Expression.evaluateNumber $ Number 3)

evaluateSimpleTextTest :: Test
evaluateSimpleTextTest =
  TestCase $ assertEqual "evaluateText returns correct text"
    "Hello World!" (Expression.evaluateText $ Text "Hello World!")
