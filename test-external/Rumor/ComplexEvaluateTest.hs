module Rumor.ComplexEvaluateTest
( tests
) where

import Rumor.Expression.Type (Expression(..), evaluate)
import Rumor.Value (Value(..))

import Data.Fixed (E12)
import Test.HUnit
import qualified Data.Text as T

tests :: Test
tests =
  TestList
    [ evaluateComplexTextExpressionTest
    ]

evaluateComplexTextExpressionTest :: Test
evaluateComplexTextExpressionTest =
  let
    expression =
      Concat ( Concat ( Concat (Text "I said \"Hello\" ")
                               (MathSubstitution (Number 3))
                      )
                      ( Concat (Text " times, ")
                               (BooleanSubstitution (Boolean True))
                      )
             )
             (Text "?")
  in
    TestCase $ assertEqual "Evaluates a complex text expression correctly"
      (TextValue "I said \"Hello\" 3 times, true?")
      (evaluate (expression :: Expression E12 T.Text))
