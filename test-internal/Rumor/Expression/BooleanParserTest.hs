module Rumor.Expression.BooleanParserTest
( tests
) where

import Rumor.Expression.Type (Expression(..))
import Rumor.Expression.Parser (boolean)
import Prelude (Bool(..), Either(..), ($))
import Data.Attoparsec.Text (parseOnly)
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
    (parseOnly boolean "false")

trueTest :: Test
trueTest =
  TestCase $ assertEqual "Parses true"
    (Right $ Boolean True)
    (parseOnly boolean "true")
