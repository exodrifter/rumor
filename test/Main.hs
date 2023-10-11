module Main where

import qualified Rumor
import qualified Rumor.Internal.Types as Rumor
import qualified Test.HUnit as HUnit

main :: IO ()
main = HUnit.runTestTTAndExit tests

tests :: HUnit.Test
tests =
  HUnit.TestList
    [ addTests
    , sayTests
    , smokeTest
    ]

addTests :: HUnit.Test
addTests =
  let
    expected speaker line =
      Right [Rumor.Add (Just (Rumor.Speaker speaker)) line]

  in
    HUnit.TestList
      [ HUnit.TestCase do
          HUnit.assertEqual "add with no speaker"
            (Right [Rumor.Add Nothing "Hello World"])
            (Rumor.parse "" "+ Hello World")

      , HUnit.TestCase do
          HUnit.assertEqual "add with unicode speaker"
            (expected "アリス" "Hello World")
            (Rumor.parse "" "アリス+ Hello World")

      , HUnit.TestCase do
          HUnit.assertEqual "add with no newline"
            (expected "alice" "Hello World")
            (Rumor.parse "" "alice+ Hello World")

      , HUnit.TestCase do
          HUnit.assertEqual "add with newline"
            (expected "alice" "Hello World")
            (Rumor.parse "" "alice+ Hello World\n")

      , HUnit.TestCase do
          HUnit.assertEqual "add with trailing whitespace"
            (expected "alice" "Hello World")
            (Rumor.parse "" "alice+ Hello World  \n")

      , HUnit.TestCase do
          HUnit.assertEqual "add with extra whitespace"
            (expected "alice" "Hello World")
            (Rumor.parse "" "alice    +    Hello World  \n")

      , HUnit.TestCase do
          HUnit.assertEqual "multiline add"
            (expected "alice" "Hello World")
            (Rumor.parse "" "alice+ Hello\n World\n")

      , HUnit.TestCase do
          HUnit.assertEqual "multiline add empty on first line"
            (expected "alice" "Hello World")
            (Rumor.parse "" "alice+\n Hello\n World\n")

      , HUnit.TestCase do
          HUnit.assertEqual "empty add"
            (expected "alice" "")
            (Rumor.parse "" "alice+")

      , HUnit.TestCase do
          HUnit.assertEqual "empty add with newline"
            (expected "alice" "")
            (Rumor.parse "" "alice+\n")

      , HUnit.TestCase do
          HUnit.assertEqual "add with a missing plus"
            ( Left "1:7:\n\
                   \  |\n\
                   \1 | alice Hello World  \n\
                   \  |       ^\n\
                   \unexpected 'H'\n\
                   \expecting '+' or ':'\n"
            )
            (Rumor.parse "" "alice Hello World  \n")
      ]

sayTests :: HUnit.Test
sayTests =
  let
    expected speaker line =
      Right [Rumor.Say (Just (Rumor.Speaker speaker)) line]

  in
    HUnit.TestList
      [ HUnit.TestCase do
          HUnit.assertEqual "say with no speaker"
            (Right [Rumor.Say Nothing "Hello World"])
            (Rumor.parse "" ": Hello World")

      , HUnit.TestCase do
          HUnit.assertEqual "say with unicode speaker"
            (expected "アリス" "Hello World")
            (Rumor.parse "" "アリス: Hello World")

      , HUnit.TestCase do
          HUnit.assertEqual "say with no newline"
            (expected "alice" "Hello World")
            (Rumor.parse "" "alice: Hello World")

      , HUnit.TestCase do
          HUnit.assertEqual "say with newline"
            (expected "alice" "Hello World")
            (Rumor.parse "" "alice: Hello World\n")

      , HUnit.TestCase do
          HUnit.assertEqual "say with trailing whitespace"
            (expected "alice" "Hello World")
            (Rumor.parse "" "alice: Hello World  \n")

      , HUnit.TestCase do
          HUnit.assertEqual "say with extra whitespace"
            (expected "alice" "Hello World")
            (Rumor.parse "" "alice    :    Hello World  \n")

      , HUnit.TestCase do
          HUnit.assertEqual "multiline say"
            (expected "alice" "Hello World")
            (Rumor.parse "" "alice: Hello\n World\n")

      , HUnit.TestCase do
          HUnit.assertEqual "multiline say empty on first line"
            (expected "alice" "Hello World")
            (Rumor.parse "" "alice:\n Hello\n World\n")

      , HUnit.TestCase do
          HUnit.assertEqual "empty say"
            (expected "alice" "")
            (Rumor.parse "" "alice:")

      , HUnit.TestCase do
          HUnit.assertEqual "empty say with newline"
            (expected "alice" "")
            (Rumor.parse "" "alice:\n")

      , HUnit.TestCase do
          HUnit.assertEqual "say with a missing colon"
            ( Left "1:7:\n\
                   \  |\n\
                   \1 | alice Hello World  \n\
                   \  |       ^\n\
                   \unexpected 'H'\n\
                   \expecting '+' or ':'\n"
            )
            (Rumor.parse "" "alice Hello World  \n")
      ]

smokeTest :: HUnit.Test
smokeTest =
  HUnit.TestCase do
    HUnit.assertEqual "smoke test 1"
      ( Right
          [ Rumor.Say (Just (Rumor.Speaker "yuu"))
              "Wah! Nanami-senpai!"
          , Rumor.Say (Just (Rumor.Speaker "touko"))
              "Yuu!"
          , Rumor.Add (Just (Rumor.Speaker "touko"))
              "Oh, thank you for finding those for us!"
          , Rumor.Say (Just (Rumor.Speaker "yuu"))
              "No problem. Umm..."
          , Rumor.Add (Just (Rumor.Speaker "yuu"))
              "Why are you coming in?"
          , Rumor.Say (Just (Rumor.Speaker "touko"))
              "Hmm?"
          ]
      )
      ( Rumor.parse ""
          "// Bloom Into You, Chapter 15: On Your Mark\n\
          \yuu: Wah! Nanami-senpai!\n\
          \touko: Yuu!\n\
          \touko+ Oh, thank you for finding those for us!\n\
          \yuu: No problem. Umm...\n\
          \yuu+ Why are you coming in?\n\
          \touko: Hmm?\n"
      )
