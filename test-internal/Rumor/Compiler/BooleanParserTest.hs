module Rumor.Compiler.BooleanParserTest
( tests
) where

import Rumor.Compiler.ExpressionParser (boolean)
import Rumor.Compiler.Helper (runParser)
import Rumor.Expression (Expression(..))

import Test.HUnit

tests :: Test
tests =
  TestList
    [ falseTest
    , trueTest
    ]

falseTest :: Test
falseTest =
  TestCase $ assertEqual "Parses false"
    (Right $ Boolean False)
    (runParser boolean "false")

trueTest :: Test
trueTest =
  TestCase $ assertEqual "Parses true"
    (Right $ Boolean True)
    (runParser boolean "true")
