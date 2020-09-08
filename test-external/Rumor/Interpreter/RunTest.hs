module Rumor.Interpreter.RunTest
( tests
) where

import Rumor
import Rumor.Interpreter.Helper (compile)

import Test.HUnit
import qualified Data.Map as Map
import qualified Data.Set as Set

tests :: Test
tests =
  TestList
    [ choiceTest
    , dialogAppendTest
    , dialogSayTest
    , stackFrameTest
    ]

choiceTest :: Test
choiceTest = TestCase $ do
  let
    c0 = compile
      "choice Red Door\n\
      \  wait\n\
      \choice Green Door\n\
      \  wait\n\
      \ "

  assertEqual "Current choices are a red or green door on init"
    ( Set.fromList
      [ "Red Door"
      , "Green Door"
      ]
    )
    (Set.fromList . fmap snd . Map.toList $ currentChoices c0)

  assertEqual "Next node is Nothing on init"
    (Nothing)
    (nextNode c0)

dialogAppendTest :: Test
dialogAppendTest = TestCase $ do
  let
    c0 = compile
      ": Hello world!\n\
      \+ How are you?\n\
      \ "
    c1 = advance c0

  assertEqual "Current dialog is \"Hello world!\" on init"
    (Just "Hello world!")
    (currentDialogFor Nothing c0)

  assertEqual "Next node is an append on init"
    (Just $ Append Nothing (Text "How are you?"))
    (nextNode c0)

  assertEqual "Current dialog after one advance is \
              \\"Hello world! How are you?\""
    (Just "Hello world! How are you?")
    (currentDialogFor Nothing c1)

  assertEqual "Next node after one advance is Nothing"
    (Nothing)
    (nextNode c1)

dialogSayTest :: Test
dialogSayTest = TestCase $ do
  let
    c0 = compile
      ": Hello world!\n\
      \: How are you?\n\
      \ "
    c1 = advance c0

  assertEqual "Current dialog is \"Hello world!\" on init"
    (Just "Hello world!")
    (currentDialogFor Nothing c0)

  assertEqual "Next node is a say on init"
    (Just $ Say Nothing (Text "How are you?"))
    (nextNode c0)

  assertEqual "Current dialog after one advance is \"How are you?\""
    (Just "How are you?")
    (currentDialogFor Nothing c1)

  assertEqual "Next node after one advance is Nothing"
    (Nothing)
    (nextNode c1)

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
    (nextNode c0)

  assertEqual "Current node after one advance is a wait"
    (Just Wait)
    (nextNode c1)

  assertEqual "Current node after two advances is Nothing"
    (Nothing)
    (nextNode c2)
