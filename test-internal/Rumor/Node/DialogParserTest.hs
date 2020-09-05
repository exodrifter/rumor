module Rumor.Node.DialogParserTest
( tests
) where

import Rumor.Expression.Type (Expression(..))
import Rumor.Node.Helper (runNodesParser)
import Rumor.Node.Type (Node(..))
import qualified Rumor.Script as Script

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
      (Right . Script.singleton $ Say Nothing (Text "Hello there!"))
      (runNodesParser ": Hello there!")

    assertEqual "Parses a single-line say with a speaker"
      (Right . Script.singleton $ Say (Just "Alice") (Text "Hello there!"))
      (runNodesParser "Alice: Hello there!")

multiLineSayTest :: Test
multiLineSayTest =
  TestCase $ do
    assertEqual "Parses a multi-line say with no speaker"
      ( Right . Script.singleton $
          Say Nothing (Text "Hello there! How are you doing?")
      )
      ( runNodesParser
          ": Hello there! \n\
          \  How are you doing? \n\
          \ "
      )

    assertEqual "Parses a multi-line say with a speaker"
      ( Right . Script.singleton $
          Say (Just "Alice") (Text "Hello there! How are you doing?")
      )
      ( runNodesParser
          "Alice: Hello there! \n\
          \       How are you doing? \n\
          \ "
      )

singleLineAppendTest :: Test
singleLineAppendTest =
  TestCase $ do
    assertEqual "Parses a single-line say with no speaker"
      ( Right . Script.singleton $
          Append Nothing (Text "Hello there!")
      )
      (runNodesParser "+ Hello there!")

    assertEqual "Parses a single-line say with a speaker"
      ( Right . Script.singleton $
          Append (Just "Alice") (Text "Hello there!")
      )
      (runNodesParser "Alice+ Hello there!")

multiLineAppendTest :: Test
multiLineAppendTest =
  TestCase $ do
    assertEqual "Parses a multi-line say with no speaker"
      ( Right . Script.singleton $
          Append Nothing (Text "Hello there! How are you doing?")
      )
      ( runNodesParser
        "+ Hello there! \n\
        \  How are you doing? \n\
        \ "
      )

    assertEqual "Parses a multi-line say with a speaker"
      ( Right . Script.singleton $
          Append (Just "Alice") (Text "Hello there! How are you doing?")
      )
      ( runNodesParser
        "Alice+ Hello there! \n\
        \       How are you doing? \n\
        \ "
      )
