module Rumor.Expression.EvaluateTest
( tests
) where

import Rumor.Prelude
import Rumor.Expression.Type (Expression(..))
import qualified Rumor.Expression.Type as Expression

import Data.Fixed (E12)
import Test.HUnit
import qualified Data.Text as T

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
    True
    (Expression.evaluateBoolean (Boolean True :: Expression E12 Bool))

evaluateSimpleNumberTest :: Test
evaluateSimpleNumberTest =
  TestCase $ assertEqual "evaluateNumber returns correct number"
    3
    (Expression.evaluateNumber (Number 3 :: Expression E12 Pico))

evaluateSimpleTextTest :: Test
evaluateSimpleTextTest =
  TestCase $ assertEqual "evaluateText returns correct text"
    "Hello World!"
    (Expression.evaluateText (Text "Hello World!" :: Expression E12 T.Text))
