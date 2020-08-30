module Rumor.Node.NodesParserTest
( tests
) where

import Rumor.Expression.Type (Expression(..))
import Rumor.Node.Helper (runNodesParser)
import Rumor.Node.Parser (nodes)
import Rumor.Node.Type (Node(..))

import Test.HUnit

tests :: Test
tests =
  TestList
    [ sayBlockTest
    ]

sayBlockTest :: Test
sayBlockTest =
  TestCase $ do
    assertEqual "Parses multiple says in a block"
      ( Right
          [ Say Nothing (Text "It was a quiet morning that day...")
          , Say (Just "Alice") (Text "Hello there! How are you doing?")
          , Say (Just "Eve") (Text "I'm doing well.")
          , Say (Just "Alice") (Text "That's great!")
          , Wait
          ]
      )
      ( runNodesParser nodes
          ": It was a quiet morning that day... \n\
          \Alice: Hello there! \n\
          \       How are you doing? \n\
          \Eve: I'm doing well. \n\
          \Alice: That's great! \n\
          \wait \n\
          \ "
      )
