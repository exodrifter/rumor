module Rumor.Compiler.DialogParserTest
( tests
) where

import Rumor.Compiler.Helper (parse, scriptSingleton)
import Rumor (Expression(..), Node(..))

import Test.HUnit

tests :: Test
tests =
  TestList
    [ singleLineSayTest
    , multiLineSayTest
    , singleLineAppendTest
    , multiLineAppendTest
    ]

singleLineSayTest :: Test
singleLineSayTest =
  TestCase $ do
    assertEqual "Parses a single-line say with no speaker"
      (Right . scriptSingleton $ Say Nothing (Text "Hello there!"))
      (parse ": Hello there!")

    assertEqual "Parses a single-line say with a speaker"
      (Right . scriptSingleton $ Say (Just "Alice") (Text "Hello there!"))
      (parse "Alice: Hello there!")

multiLineSayTest :: Test
multiLineSayTest =
  TestCase $ do
    assertEqual "Parses a multi-line say with no speaker"
      ( Right . scriptSingleton $
          Say Nothing (Text "Hello there! How are you doing?")
      )
      ( parse
          ": Hello there! \n\
          \  How are you doing? \n\
          \ "
      )

    assertEqual "Parses a multi-line say with a speaker"
      ( Right . scriptSingleton $
          Say (Just "Alice") (Text "Hello there! How are you doing?")
      )
      ( parse
          "Alice: Hello there! \n\
          \       How are you doing? \n\
          \ "
      )

singleLineAppendTest :: Test
singleLineAppendTest =
  TestCase $ do
    assertEqual "Parses a single-line say with no speaker"
      ( Right . scriptSingleton $
          Append Nothing (Text "Hello there!")
      )
      (parse "+ Hello there!")

    assertEqual "Parses a single-line say with a speaker"
      ( Right . scriptSingleton $
          Append (Just "Alice") (Text "Hello there!")
      )
      (parse "Alice+ Hello there!")

multiLineAppendTest :: Test
multiLineAppendTest =
  TestCase $ do
    assertEqual "Parses a multi-line say with no speaker"
      ( Right . scriptSingleton $
          Append Nothing (Text "Hello there! How are you doing?")
      )
      ( parse
        "+ Hello there! \n\
        \  How are you doing? \n\
        \ "
      )

    assertEqual "Parses a multi-line say with a speaker"
      ( Right . scriptSingleton $
          Append (Just "Alice") (Text "Hello there! How are you doing?")
      )
      ( parse
        "Alice+ Hello there! \n\
        \       How are you doing? \n\
        \ "
      )
