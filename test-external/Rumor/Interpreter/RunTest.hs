module Rumor.Interpreter.RunTest
( tests
) where

import Rumor (Node(..), advance, currentNode)
import Rumor.Interpreter.Helper (compile)

import Test.HUnit

tests :: Test
tests =
  TestList
    [ stackFrameTest
    ]

stackFrameTest :: Test
stackFrameTest = TestCase $ do
  let
    c0 = compile
      "label [a] \n\
      \  wait \n\
      \label [b] \n\
      \  wait \n\
      \jump a \n\
      \jump b \n\
      \ "
    c1 = advance c0
    c2 = advance c1

  assertEqual "Current initial node is a wait"
    (Just Wait)
    (currentNode c0)

  assertEqual "Current node after one advance is a wait"
    (Just Wait)
    (currentNode c1)

  assertEqual "Current node after two advances is Nothing"
    (Nothing)
    (currentNode c2)
