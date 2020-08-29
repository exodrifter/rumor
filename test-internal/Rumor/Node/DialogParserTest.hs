module Rumor.Node.DialogParserTest
( tests
) where

import Rumor.Expression.Type (Expression(..))
import Rumor.Node.Helper (runNodeParser)
import Rumor.Node.Parser (append, say)
import Rumor.Node.Type (Node(..))

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
      (Right $ Say Nothing (Text "Hello there!"))
      (runNodeParser say ": Hello there!")

    assertEqual "Parses a single-line say with a speaker"
      (Right $ Say (Just "Alice") (Text "Hello there!"))
      (runNodeParser say "Alice: Hello there!")

multiLineSayTest :: Test
multiLineSayTest =
  TestCase $ do
    assertEqual "Parses a multi-line say with no speaker"
      (Right $ Say Nothing (Text "Hello there! How are you doing?"))
      ( runNodeParser say
          ": Hello there! \n\
          \  How are you doing? \n\
          \ "
      )

    assertEqual "Parses a multi-line say with a speaker"
      (Right $ Say (Just "Alice") (Text "Hello there! How are you doing?"))
      ( runNodeParser say
          "Alice: Hello there! \n\
          \       How are you doing? \n\
          \ "
      )

singleLineAppendTest :: Test
singleLineAppendTest =
  TestCase $ do
    assertEqual "Parses a single-line say with no speaker"
      (Right $ Append Nothing (Text "Hello there!"))
      (runNodeParser append "+ Hello there!")

    assertEqual "Parses a single-line say with a speaker"
      (Right $ Append (Just "Alice") (Text "Hello there!"))
      (runNodeParser append "Alice+ Hello there!")

multiLineAppendTest :: Test
multiLineAppendTest =
  TestCase $ do
    assertEqual "Parses a multi-line say with no speaker"
      (Right $ Append Nothing (Text "Hello there! How are you doing?"))
      ( runNodeParser append
        "+ Hello there! \n\
        \  How are you doing? \n\
        \ "
      )

    assertEqual "Parses a multi-line say with a speaker"
      (Right $ Append (Just "Alice") (Text "Hello there! How are you doing?"))
      ( runNodeParser append
        "Alice+ Hello there! \n\
        \       How are you doing? \n\
        \ "
      )
