module Rumor.Expression.SimplifyTest
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
    [ simplifySimpleBooleanTest

    -- Number tests
    , simplifySimpleNumberTest

    -- Text tests
    , simplifySimpleTextTest
    , simplifyLiteralBooleanSubstitutionTest
    , simplifyLiteralNumberSubstitutionTest
    , simplifyTextConcatenationTest
    ]

simplifySimpleBooleanTest :: Test
simplifySimpleBooleanTest =
  TestCase $ assertEqual "simplifyBool returns correct boolean"
    (Boolean True)
    (Expression.simplifyBoolean $ Boolean True)

simplifySimpleNumberTest :: Test
simplifySimpleNumberTest =
  TestCase $ assertEqual "simplifyNumber returns correct number"
    (Number 3)
    (Expression.simplifyNumber $ Number 3)

simplifySimpleTextTest :: Test
simplifySimpleTextTest =
  TestCase $ assertEqual "simplifyText returns correct text"
    (Text "Hello World!")
    (Expression.simplifyText $ Text "Hello World!")

simplifyLiteralBooleanSubstitutionTest :: Test
simplifyLiteralBooleanSubstitutionTest =
  TestCase $ assertEqual "Simplifies literal boolean substitution"
    (Text "That is true")
    (Expression.simplifyText $
      Concat (Text "That is ") (BooleanSubstitution (Boolean True))
    )

simplifyLiteralNumberSubstitutionTest :: Test
simplifyLiteralNumberSubstitutionTest =
  TestCase $ assertEqual "Simplifies literal number substitution"
    (Text "The value is 3.0")
    (Expression.simplifyText $
      Concat (Text "The value is ") (NumberSubstitution (Number 3))
    )

simplifyTextConcatenationTest :: Test
simplifyTextConcatenationTest =
  TestCase $ assertEqual "Simplifies text concatenation"
    (Text "Hello World!")
    (Expression.simplifyText $
      Concat (Text "Hello ") (Text "World!")
    )
