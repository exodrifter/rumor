module Rumor.Expression.NumberParserTest
( tests
) where

import Rumor.Expression.Type (Expression(..))
import Rumor.Expression.Parser (number)
import Prelude (Either(..), ($))
import Data.Attoparsec.Text (parseOnly)
import Test.HUnit

tests :: Test
tests =
  TestList
    [ integerTest
    , fractionalTest
    ]

integerTest :: Test
integerTest =
  TestCase $ assertEqual "Parses an integer"
    (Right $ Number 3)
    (parseOnly number "3")

fractionalTest :: Test
fractionalTest =
  TestCase $ assertEqual "Parses a fractional number"
    (Right $ Number 90.5)
    (parseOnly number "90.5")
