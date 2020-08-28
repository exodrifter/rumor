module Rumor.Expression.SimplifyTest
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
    [ simplifySimpleBooleanTest

    -- Number tests
    , simplifySimpleNumberTest
    , simplifyAdditionTest
    , simplifySubtractionTest
    , simplifyMultiplicationTest
    , simplifyDivisionTest
    , simplifyNestedMathTest

    -- Text tests
    , simplifySimpleTextTest
    , simplifyLiteralBooleanSubstitutionTest
    , simplifyLiteralMathSubstitutionTest
    , simplifyTextConcatenationTest
    ]

simplifySimpleBooleanTest :: Test
simplifySimpleBooleanTest =
  TestCase $ assertEqual "simplifyBool returns correct boolean"
    (Boolean True :: Expression E12 Bool)
    (Expression.simplifyBoolean $ Boolean True)

simplifySimpleNumberTest :: Test
simplifySimpleNumberTest =
  TestCase $ assertEqual "simplifyMath returns correct number"
    (Number 3 :: Expression E12 Pico)
    (Expression.simplifyMath $ Number 3)

simplifyAdditionTest :: Test
simplifyAdditionTest =
  TestCase $ assertEqual "Simplifies addition"
    (Number 15 :: Expression E12 Pico)
    (Expression.simplifyMath $ Add (Number 5) (Number 10))

simplifySubtractionTest :: Test
simplifySubtractionTest =
  TestCase $ assertEqual "Simplifies subtraction"
    (Number (-5) :: Expression E12 Pico)
    (Expression.simplifyMath $ Subtract (Number 5) (Number 10))

simplifyMultiplicationTest :: Test
simplifyMultiplicationTest =
  TestCase $ assertEqual "Simplifies multiplication"
    (Number 50 :: Expression E12 Pico)
    (Expression.simplifyMath $ Multiply (Number 5) (Number 10))

simplifyDivisionTest :: Test
simplifyDivisionTest =
  TestCase $ assertEqual "Simplifies division"
    (Number 0.5 :: Expression E12 Pico)
    (Expression.simplifyMath $ Divide (Number 5) (Number 10))

simplifyNestedMathTest :: Test
simplifyNestedMathTest =
  TestCase $ assertEqual "Simplifies nested mathematical operations"
    (Number 2 :: Expression E12 Pico)
    (Expression.simplifyMath $
      Divide (Add (Number 5) (Number 5))
             (Subtract (Number 10) (Number 5))
    )

simplifySimpleTextTest :: Test
simplifySimpleTextTest =
  TestCase $ assertEqual "simplifyText returns correct text"
    (Text "Hello World!" :: Expression E12 T.Text)
    (Expression.simplifyText $ Text "Hello World!")

simplifyLiteralBooleanSubstitutionTest :: Test
simplifyLiteralBooleanSubstitutionTest =
  TestCase $ assertEqual "Simplifies literal boolean substitution"
    (Text "That is true" :: Expression E12 T.Text)
    (Expression.simplifyText $
      Concat (Text "That is ") (BooleanSubstitution (Boolean True))
    )

simplifyLiteralMathSubstitutionTest :: Test
simplifyLiteralMathSubstitutionTest =
  TestCase $ assertEqual "Simplifies literal number substitution"
    (Text "The value is 3" :: Expression E12 T.Text)
    (Expression.simplifyText $
      Concat (Text "The value is ") (MathSubstitution (Number 3))
    )

simplifyTextConcatenationTest :: Test
simplifyTextConcatenationTest =
  TestCase $ assertEqual "Simplifies text concatenation"
    (Text "Hello World!" :: Expression E12 T.Text)
    (Expression.simplifyText $
      Concat (Text "Hello ") (Text "World!")
    )
