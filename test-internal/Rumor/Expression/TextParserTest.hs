module Rumor.Expression.TextParserTest
( tests
) where

import Prelude (Either(..), ($))
import Rumor.Expression.Parser (text)
import Rumor.Expression.Type (Expression(..))
import Rumor.Helper(runTestParser)
import Test.HUnit

tests :: Test
tests =
  TestList
    [ textTest
    , textEscapeTest
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
    (runTestParser text "\"Hello World!\"")

textWhitespaceTest :: Test
textWhitespaceTest =
  TestCase $ assertEqual "Collapses redundant whitespace in text"
    (Right $ Text " Hello World ! ")
    (runTestParser text "\"\n\t  Hello\n World \t!   \t\n  \"")

textEscapeTest :: Test
textEscapeTest =
  TestCase $ assertEqual "Parses text containing escape sequences"
    (Right $ Text "{ \\ \"") -- { \ "
    (runTestParser text "\"\\{ \\\\ \\\"\"") -- "\{ \\ \""

booleanSubstitutionTest :: Test
booleanSubstitutionTest =
  TestCase $ assertEqual "Parses text containing a boolean substitution"
    (Right $ Text "This statement is true.")
    (runTestParser text "\"This statement is {true}.\"")

numberSubstitutionTest :: Test
numberSubstitutionTest =
  TestCase $ assertEqual "Parses text containing a number substitution"
    (Right $ Text "I have 3 apples.")
    (runTestParser text "\"I have {3} apples.\"")

mathSubstitutionTest :: Test
mathSubstitutionTest =
  TestCase $ assertEqual "Parses text containing a math substitution"
    (Right $ Text "I have 3 apples.")
    (runTestParser text "\"I have {(1+1)+1} apples.\"")

textSubstitutionTest :: Test
textSubstitutionTest =
  TestCase $ assertEqual "Parses text containing a string substitution"
    (Right $ Text "Hello World!")
    (runTestParser text "\"Hello {\"World\"}!\"")

nestedSubstitutionTest :: Test
nestedSubstitutionTest =
  TestCase $ assertEqual "Parses text containing nested substitutions"
    (Right $ Text "Foo Bar Baz!")
    (runTestParser text "\"Foo {\"Bar {\"Baz\"}\"}!\"")

paddedWhitespaceSubstitutionTest :: Test
paddedWhitespaceSubstitutionTest =
  TestCase $ assertEqual "Parses a substitution padded with whitespace"
    (Right $ Text "Hello World!")
    (runTestParser text "\"Hello {\n \"World\" \t}!\"")
