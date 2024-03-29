module Rumor.ComplexSimplifyTest
( tests
) where

import Rumor (Expression(..), simplify)

import Data.Fixed (E12)
import Test.HUnit
import qualified Data.Text as T

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
                               (MathSubstitution (Add (Number 1) (Number 2)))
                      )
                      ( Concat (Text " times, \r\n")
                               (BooleanSubstitution (Boolean True))
                      )
             )
             (Text "?")
  in
    TestCase $ assertEqual "Simplifies a complex text expression correctly"
      (Text "I said \"Hello\" 3 times, true?")
      (simplify (expression :: Expression E12 T.Text))
