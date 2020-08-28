module Rumor.Expression.NumberParserTest
( tests
) where

import Rumor.Expression.Parser (math)
import Rumor.Expression.Type (Expression(..))
import Rumor.Helper (runTestParser)

import Test.HUnit

tests :: Test
tests =
  TestList
    [ integerTest
    , fractionalTest
    , additionTest
    , subtractionTest
    , multiplicationTest
    , divisionTest
    , operationPrecedenceTest
    , chainTest
    , parenthesisTest
    , mathWhitespaceTest
    ]

integerTest :: Test
integerTest =
  TestCase $ do
    assertEqual "Parses an integer"
      (Right $ Number 3)
      (runTestParser math "3")
    assertEqual "Parses a negative integer"
      (Right $ Number (-3))
      (runTestParser math "-3")

fractionalTest :: Test
fractionalTest =
  TestCase $ do
    assertEqual "Parses a fractional number"
      (Right $ Number 90.5)
      (runTestParser math "90.5")

    assertEqual "Parses a negative fractional number"
      (Right $ Number (-90.5))
      (runTestParser math "-90.5")

additionTest :: Test
additionTest =
  TestCase $ do
    assertEqual "Parses addition"
      (Right $ Number 15)
      (runTestParser math "5+10")
    assertEqual "Parses addition with negative numbers"
      (Right $ Number (-15))
      (runTestParser math "-5+-10")
    assertEqual "Parses addition with a negative number on the right"
      (Right $ Number (-5))
      (runTestParser math "5+-10")
    assertEqual "Parses addition with a negative number on the left"
      (Right $ Number 5)
      (runTestParser math "-5+10")

subtractionTest :: Test
subtractionTest =
  TestCase $ do
    assertEqual "Parses subtraction"
      (Right $ Number (-5))
      (runTestParser math "5-10")
    assertEqual "Parses subtraction with negative numbers"
      (Right $ Number 5)
      (runTestParser math "-5--10")
    assertEqual "Parses subtraction with a negative number on the right"
      (Right $ Number 15)
      (runTestParser math "5--10")
    assertEqual "Parses subtraction with a negative number on the left"
      (Right $ Number (-15))
      (runTestParser math "-5-10")

multiplicationTest :: Test
multiplicationTest =
  TestCase $ do
    assertEqual "Parses multiplication"
      (Right $ Number 50)
      (runTestParser math "5*10")
    assertEqual "Parses multiplication with negative numbers"
      (Right $ Number 50)
      (runTestParser math "-5*-10")
    assertEqual "Parses multiplication with a negative number on the right"
      (Right $ Number (-50))
      (runTestParser math "5*-10")
    assertEqual "Parses multiplication with a negative number on the left"
      (Right $ Number (-50))
      (runTestParser math "-5*10")

divisionTest :: Test
divisionTest =
  TestCase $ do
    assertEqual "Parses division"
      (Right $ Number 0.5)
      (runTestParser math "5/10")
    assertEqual "Parses division with negative numbers"
      (Right $ Number 0.5)
      (runTestParser math "-5/-10")
    assertEqual "Parses division with a negative number on the right"
      (Right $ Number (-0.5))
      (runTestParser math "5/-10")
    assertEqual "Parses division with a negative number on the left"
      (Right $ Number (-0.5))
      (runTestParser math "-5/10")

operationPrecedenceTest :: Test
operationPrecedenceTest =
  TestCase $ assertEqual "Parses operators using the correct precedence"
    (Right $ Number 36)
    (runTestParser math "5+3*10+2/2")

chainTest :: Test
chainTest =
  TestCase $ assertEqual "Parses chained operators with the correct precendence"
    (Right $ Number (-48))
    (runTestParser math "1*2*3/3+10-10*2*3")

parenthesisTest :: Test
parenthesisTest =
  TestCase $ assertEqual "Parses mathematical expressions using parenthesis"
    (Right $ Number 48)
    (runTestParser math "(5+3)*(10+2)/2")

mathWhitespaceTest :: Test
mathWhitespaceTest =
  TestCase $ assertEqual "Parses mathematical expressions containing whitespace"
      (Right $ Number 48)
      (runTestParser math "(5\t+\n3) *   (\n\n\t  10\t +\n 2 \t) /\n2")
