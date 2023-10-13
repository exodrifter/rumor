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
    , interpolationTests
    , smokeTest
    ]

addTests :: HUnit.Test
addTests =
  let
    expected speaker line =
      Right [Rumor.Add (Just (Rumor.Speaker speaker)) (Rumor.String line)]

  in
    HUnit.TestList
      [ HUnit.TestCase do
          HUnit.assertEqual "add with no speaker"
            (Right [Rumor.Add Nothing (Rumor.String "Hello World")])
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
          HUnit.assertEqual "multiline add multiple paragraphs"
            (expected "alice" "Hello World")
            (Rumor.parse "" "alice+\n Hello\n\n World\n")

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
      Right [Rumor.Say (Just (Rumor.Speaker speaker)) (Rumor.String line)]

  in
    HUnit.TestList
      [ HUnit.TestCase do
          HUnit.assertEqual "say with no speaker"
            (Right [Rumor.Say Nothing (Rumor.String "Hello World")])
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
          HUnit.assertEqual "multiline say multiple paragraphs"
            (expected "alice" "Hello World")
            (Rumor.parse "" "alice:\n Hello\n\n World\n")

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

interpolationTests :: HUnit.Test
interpolationTests =
  let
    expected line =
      Right [Rumor.Say Nothing (Rumor.String line)]
  in
    HUnit.TestList
      [ HUnit.TestCase do
          HUnit.assertEqual "iterpolation"
            (expected "Hello World")
            (Rumor.parse "" ": Hello {\"World\"}\n")

      , HUnit.TestCase do
          HUnit.assertEqual "standalone iterpolation"
            (expected "Hello World")
            (Rumor.parse "" ": {\"Hello World\"}\n")

      , HUnit.TestCase do
          HUnit.assertEqual "iterpolation with whitespace"
            (expected "Hello  World")
            (Rumor.parse "" ": {  \"Hello  World\"  }\n")

      , HUnit.TestCase do
          HUnit.assertEqual "iterpolation with newlines"
            (expected "Hello World")
            (Rumor.parse "" ": {\n\"Hello World\"\n}\n")

      , HUnit.TestCase do
          HUnit.assertEqual "iterpolation in an interpolation"
            (expected " Hello World ")
            (Rumor.parse "" ": {\" {\"Hello World\"} \"}\n")

      , HUnit.TestCase do
          HUnit.assertEqual "string interpolation with unexpected newline"
            ( Left "1:13:\n\
                   \  |\n\
                   \1 | : {\" {\"Hello\n  |             ^\n\
                   \unexpected newline\n\
                   \expecting '\\', '{', end double quote, end of input, or literal char\n"
            )
            (Rumor.parse "" ": {\" {\"Hello\nWorld\"} \"}\n")
      ]

smokeTest :: HUnit.Test
smokeTest =
  HUnit.TestCase do
    let mkDialog cons speaker line =
          cons (Just (Rumor.Speaker speaker)) (Rumor.String line)

    HUnit.assertEqual "smoke test 1"
      ( Right
          [ mkDialog Rumor.Say "yuu"
              "Wah! Nanami-senpai!"
          , mkDialog Rumor.Say "touko"
              "Yuu!"
          , mkDialog Rumor.Add "touko"
              "Oh, thank you for finding those for us!"
          , mkDialog Rumor.Say "yuu"
              "No problem. Umm..."
          , mkDialog Rumor.Add "yuu"
              "Why are you coming in?"
          , mkDialog Rumor.Say "touko"
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
