module Rumor.Compiler.TextParserTest
( tests
) where

import Rumor.Compiler.ExpressionParser (text)
import Rumor.Compiler.Helper (runParser)
import Rumor.Expression (Expression(..))

import Test.HUnit

tests :: Test
tests =
  TestList
    [ textTest
    , textWhitespaceTest
    , booleanSubstitutionTest
    , numberSubstitutionTest
    , mathSubstitutionTest
    , textSubstitutionTest
    , nestedSubstitutionTest
    , paddedWhitespaceSubstitutionTest
    ]

textTest :: Test
textTest =
  TestCase $ assertEqual "Parses simple text"
    (Right $ Text "Hello World!")
    (runParser text "Hello World!")

textWhitespaceTest :: Test
textWhitespaceTest =
  TestCase $ assertEqual "Collapses redundant whitespace in text"
    (Right $ Text "Hello World !")
    (runParser text "\n\t  Hello\n World \t!   \t\n  ")

booleanSubstitutionTest :: Test
booleanSubstitutionTest =
  TestCase $ assertEqual "Parses text containing a boolean substitution"
    (Right $ Text "This statement is true.")
    (runParser text "This statement is {true}.")

numberSubstitutionTest :: Test
numberSubstitutionTest =
  TestCase $ assertEqual "Parses text containing a number substitution"
    (Right $ Text "I have 3 apples.")
    (runParser text "I have {3} apples.")

mathSubstitutionTest :: Test
mathSubstitutionTest =
  TestCase $ assertEqual "Parses text containing a math substitution"
    (Right $ Text "I have 3 apples.")
    (runParser text "I have {(1+1)+1} apples.")

textSubstitutionTest :: Test
textSubstitutionTest =
  TestCase $ assertEqual "Parses text containing a string substitution"
    (Right $ Text "Hello World!")
    (runParser text "Hello {\"World\"}!")

nestedSubstitutionTest :: Test
nestedSubstitutionTest =
  TestCase $ assertEqual "Parses text containing nested substitutions"
    (Right $ Text "Foo Bar Baz!")
    (runParser text "Foo {\"Bar {\"Baz\"}\"}!")

paddedWhitespaceSubstitutionTest :: Test
paddedWhitespaceSubstitutionTest =
  TestCase $ assertEqual "Parses a substitution padded with whitespace"
    (Right $ Text "Hello World!")
    (runParser text "Hello {\n \"World\" \t}!")
