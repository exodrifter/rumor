module Rumor.Expression.BooleanParserTest
( tests
) where

import Prelude (Bool(..), Either(..), ($))
import Rumor.Expression.Parser (boolean)
import Rumor.Expression.Type (Expression(..))
import Rumor.Helper (runTestParser)
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
    (runTestParser boolean "false")

trueTest :: Test
trueTest =
  TestCase $ assertEqual "Parses true"
    (Right $ Boolean True)
    (runTestParser boolean "true")
