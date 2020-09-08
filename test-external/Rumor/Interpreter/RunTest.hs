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
    , pauseTest
    , stackFrameTest
    ]

choiceTest :: Test
choiceTest = TestCase $ do
  let c0 = compile
        "choice [red] Red Door \n\
        \  : Hello! \n\
        \choice [green] Green Door \n\
        \  wait \n\
        \choose \n\
        \ "

  assertEqual "Current choices are a red or green door on init"
    ( Set.fromList
      [ "Red Door"
      , "Green Door"
      ]
    )
    (Set.fromList . fmap snd . Map.toList $ currentChoices c0)

  assertEqual "Next node is a choose on init"
    (Just Choose)
    (nextNode c0)

  let c1 = choose "blue" c0
  assertEqual "Next node is still a choose after selecting an invalid choice"
    (Just Choose)
    (nextNode c1)

  let c2 = choose "green" c1
  assertEqual "Next node is a wait after selecting a valid choice"
    (Just Wait)
    (nextNode c2)

dialogAppendTest :: Test
dialogAppendTest = TestCase $ do
  let c0 = compile
        ": Hello world!\n\
        \+ How are you?\n\
        \ "

  assertEqual "Current dialog is \"Hello world!\" on init"
    (Just "Hello world!")
    (currentDialogFor Nothing c0)

  assertEqual "Next node is an append on init"
    (Just $ Append Nothing (Text "How are you?"))
    (nextNode c0)

  let c1 = advance c0
  assertEqual "Current dialog after one advance is \
              \\"Hello world! How are you?\""
    (Just "Hello world! How are you?")
    (currentDialogFor Nothing c1)

  assertEqual "Next node after one advance is Nothing"
    (Nothing)
    (nextNode c1)

dialogSayTest :: Test
dialogSayTest = TestCase $ do
  let c0 = compile
        ": Hello world!\n\
        \: How are you?\n\
        \ "

  assertEqual "Current dialog is \"Hello world!\" on init"
    (Just "Hello world!")
    (currentDialogFor Nothing c0)

  assertEqual "Next node is a say on init"
    (Just $ Say Nothing (Text "How are you?"))
    (nextNode c0)

  let c1 = advance c0
  assertEqual "Current dialog after one advance is \"How are you?\""
    (Just "How are you?")
    (currentDialogFor Nothing c1)

  assertEqual "Next node after one advance is Nothing"
    (Nothing)
    (nextNode c1)

pauseTest :: Test
pauseTest = TestCase $ do
  let c0 = compile
        "pause 5 seconds \n\
        \ "

  assertEqual "Current initial node is a pause"
    (Just (Pause (Number 5000)))
    (nextNode c0)

  let c1 = advance c0
  assertEqual "Current node after one advance is still a pause"
    (Just (Pause (Number 5000)))
    (nextNode c1)

  let c2 = update 3000 c1
  assertEqual "Current node after waiting 3 seconds is still a pause"
    (Just (Pause (Number 5000)))
    (nextNode c2)

  let c3 = update 3000 c2
  assertEqual "Current node after waiting another 3 seconds is Nothing"
    (Nothing)
    (nextNode c3)

stackFrameTest :: Test
stackFrameTest = TestCase $ do
  let c0 = compile
        "label [a] \n\
        \  wait \n\
        \label [b] \n\
        \  wait \n\
        \jump a \n\
        \jump b \n\
        \ "

  assertEqual "Current initial node is a wait"
    (Just Wait)
    (nextNode c0)

  let c1 = advance c0
  assertEqual "Current node after one advance is a wait"
    (Just Wait)
    (nextNode c1)

  let c2 = advance c1
  assertEqual "Current node after two advances is Nothing"
    (Nothing)
    (nextNode c2)
