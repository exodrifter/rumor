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
    [ nodesTest
    , sayBlockTest
    , identifierDuplicateTest
    ]

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
          , Clear All
          , Clear Dialog
          , Clear Choices
          , Choose
          , Choice (Just "foo") (Text "Green Door")
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
          \choice [foo] Green Door \n\
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