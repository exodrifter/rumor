module Rumor.ComplexEvaluateTest
( tests
) where

import Rumor.Expression.Type (Expression(..), evaluate)
import Rumor.Value (Value(..))
import Prelude (Bool(..), ($))
import Test.HUnit

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
                               (NumberSubstitution (Number 3))
                      )
                      ( Concat (Text " times, ")
                               (BooleanSubstitution (Boolean True))
                      )
             )
             (Text "?")
  in
    TestCase $ assertEqual "Evaluates a complex text expression correctly"
      (TextValue "I said \"Hello\" 3.0 times, true?")
      (evaluate expression)
