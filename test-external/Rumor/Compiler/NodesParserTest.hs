module Rumor.Compiler.NodesParserTest
( tests
) where

import Rumor.Compiler.Helper (parse)
import Rumor (ClearFlag(..), Expression(..), Node(..), Script(..))

import Data.Either (isLeft)
import Test.HUnit
import qualified Data.Map.Strict as Map

tests :: Test
tests =
  TestList
    [ identifierDuplicateTest
    , nodesTest
    , noSpeakerTest
    , sayBlockTest
    ]

identifierDuplicateTest :: Test
identifierDuplicateTest =
  TestCase $ do
    let result = parse
          "label [foo] \n\
          \  : Hello \n\
          \label [foo] \n\
          \  : Hello \n\
          \ "
    assertBool "Fails to parse nodes that reuse identifiers"
      (isLeft result)

nodesTest :: Test
nodesTest =
  TestCase $ do
    assertEqual "Parses multiple nodes in a block"
      ( Right Script
        { sections = Map.empty
        , nodes =
          [ Say Nothing (Text "Hello World!")
          , Append Nothing (Text "Hello World!")
          , Wait
          , Pause (Number 2.5)
          , Pause (Number 2500)
          , Pause (Number 150000)
          , Jump "bizzbazz"
          , Return
          , Clear ClearAll
          , Clear ClearDialog
          , Clear ClearChoices
          , Choose
          , Choice "_7/JG9dW4r4RiUuBy2e2RuimxP+s=" (Text "Red Door")
          , Choice "foo" (Text "Green Door")
          ]
        }
      )
      ( parse
          ": Hello World! \n\
          \+ Hello World! \n\
          \wait \n\
          \pause 2.5 milliseconds \n\
          \pause 2.5 seconds \n\
          \pause 2.5 minutes \n\
          \jump bizzbazz \n\
          \return \n\
          \clear all \n\
          \clear dialog \n\
          \clear choices \n\
          \choose \n\
          \choice Red Door \n\
          \choice [foo] Green Door \n\
          \ "
      )

noSpeakerTest :: Test
noSpeakerTest =
  TestCase $ do
    assertEqual "A dialog statement with no speaker correctly"
      ( Right Script
        { sections = Map.empty
        , nodes =
          [ Wait
          , Say Nothing (Text "Hello!")
          ]
        }
      )
      ( parse
          "wait \n\
          \: Hello! \n\
          \ "
      )

sayBlockTest :: Test
sayBlockTest =
  TestCase $ do
    assertEqual "Parses multiple says in a block"
      ( Right Script
        { sections = Map.empty
        , nodes =
          [ Say Nothing (Text "It was a quiet morning that day...")
          , Say (Just "Alice") (Text "Hello there! How are you doing?")
          , Say (Just "Eve") (Text "I'm doing well.")
          , Say (Just "Alice") (Text "That's great!")
          ]
        }
      )
      ( parse
          ": It was a quiet morning that day... \n\
          \Alice: Hello there! \n\
          \       How are you doing? \n\
          \Eve: I'm doing well. \n\
          \Alice: That's great! \n\
          \ "
      )
