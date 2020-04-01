module Rumor.ComplexSimplifyTest
( tests
) where

import Rumor.Expression.Type (Expression(..), simplify)
import Prelude (Bool(..), ($))
import Test.HUnit

tests :: Test
tests =
  TestList
    [ simplifyComplexTextExpressionTest
    ]

simplifyComplexTextExpressionTest :: Test
simplifyComplexTextExpressionTest =
  let
    expression =
      Concat ( Concat ( Concat (Text "I    said \t \"Hello\" \n ")
                               (NumberSubstitution (Number 3))
                      )
                      ( Concat (Text " times, \r\n")
                               (BooleanSubstitution (Boolean True))
                      )
             )
             (Text "?")
  in
    TestCase $ assertEqual "Simplifies a complex text expression correctly"
      (Text "I said \"Hello\" 3.0 times, true?")
      (simplify expression)
