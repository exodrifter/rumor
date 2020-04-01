module Rumor.Expression.NumberParserTest
( tests
) where

import Rumor.Expression.Type (Expression(..))
import Rumor.Expression.Parser (math)
import Prelude (Either(..), ($))
import Data.Attoparsec.Text (parseOnly)
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
    , parenthesisTest
    , mathWhitespaceTest
    ]

integerTest :: Test
integerTest =
  TestCase $ assertEqual "Parses an integer"
    (Right $ Number 3)
    (parseOnly math "3")

fractionalTest :: Test
fractionalTest =
  TestCase $ assertEqual "Parses a fractional number"
    (Right $ Number 90.5)
    (parseOnly math "90.5")

additionTest :: Test
additionTest =
  TestCase $ assertEqual "Parses addition"
    (Right $ Number 15)
    (parseOnly math "5+10")

subtractionTest :: Test
subtractionTest =
  TestCase $ assertEqual "Parses subtraction"
    (Right $ Number (-5))
    (parseOnly math "5-10")

multiplicationTest :: Test
multiplicationTest =
  TestCase $ assertEqual "Parses multiplication"
    (Right $ Number 50)
    (parseOnly math "5*10")

divisionTest :: Test
divisionTest =
  TestCase $ assertEqual "Parses division"
    (Right $ Number 0.5)
    (parseOnly math "5/10")

operationPrecedenceTest :: Test
operationPrecedenceTest =
  TestCase $ assertEqual "Parses operators using the correct precedence"
    (Right $ Number 36)
    (parseOnly math "5+3*10+2/2")

parenthesisTest :: Test
parenthesisTest =
  TestCase $ assertEqual "Parses mathematical expressions using parenthesis"
    (Right $ Number 48)
    (parseOnly math "(5+3)*(10+2)/2")

mathWhitespaceTest :: Test
mathWhitespaceTest =
  TestCase $ assertEqual "Parses mathematical expressions containing whitespace"
      (Right $ Number 48)
      (parseOnly math "(5\t+\n3) *   (\n\n\t  10\t +\n 2 \t) /\n2")
