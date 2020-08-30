module Rumor.Node.SectionTest
( tests
) where

import Rumor.Expression.Type (Expression(..))
import Rumor.Node.Helper (runNodeParser, runNodesParser)
import Rumor.Node.Parser (nodes, section)
import Rumor.Node.Type (Node(..))

import Test.HUnit
import qualified Data.List.NonEmpty as NE

tests :: Test
tests =
  TestList
    [ sectionTest
    , multiSectionTest
    , nestedSectionTest
    , multiNestedSectionTest
    ]

sectionTest :: Test
sectionTest =
  TestCase $ assertEqual "Parses a single section"
    ( Right .
        Section "foobar" $ NE.fromList
          [ Say Nothing (Text "Hello world!")
          , Say Nothing (Text "Fizz bazz!")
          ]
    )
    ( runNodeParser section
        "label foobar \n\
        \  : Hello world! \n\
        \  : Fizz bazz! \n\
        \ "
    )

multiSectionTest :: Test
multiSectionTest =
  TestCase $ assertEqual "Parses multiple sections"
    ( Right
      [ Section "foo" $ NE.fromList
        [ Say Nothing (Text "Hello world! Hello everyone!")
        ]
      , Section "bar" $ NE.fromList
        [ Say Nothing (Text "Fizz bazz!")
        , Say Nothing (Text "Bazz fizz!")
        ]
      ]
    )
    ( runNodesParser nodes
        "label foo \n\
        \  : Hello world! \n\
        \    Hello everyone! \n\
        \label bar \n\
        \  : Fizz bazz! \n\
        \  : Bazz fizz! \n\
        \ "
    )

nestedSectionTest :: Test
nestedSectionTest =
  TestCase $ assertEqual "Parses a nested section"
    ( Right $
        Section "foo" $ NE.fromList
          [ Section "bar" $ NE.fromList
            [ Say Nothing (Text "Hello world!")
            ]
          ]
    )
    ( runNodeParser section
        "label foo \n\
        \  label bar \n\
        \    : Hello world! \n\
        \ "
    )

multiNestedSectionTest :: Test
multiNestedSectionTest =
  TestCase $ assertEqual "Parses multiple nested sections"
    ( Right
      [ Section "foo" $ NE.fromList
        [ Say Nothing (Text "Hello world! Hello everyone!")
        , Section "bar" $ NE.fromList
          [ Say Nothing (Text "Fizz bazz!")
          , Say Nothing (Text "Bazz fizz!")
          ]
        ]
      , Section "bizz" $ NE.fromList
        [ Append Nothing (Text "Hello world!")
        , Append Nothing (Text "Hello everyone!")
        , Section "bazz" $ NE.fromList
          [ Append Nothing (Text "Fizz bazz! Bazz fizz!")
          ]
        ]
      ]
    )
    ( runNodesParser nodes
        "label foo \n\
        \  : Hello world! \n\
        \    Hello everyone! \n\
        \  label bar \n\
        \    : Fizz bazz! \n\
        \    : Bazz fizz! \n\
        \label bizz \n\
        \  + Hello world! \n\
        \  + Hello everyone! \n\
        \  label bazz \n\
        \    + Fizz bazz! \n\
        \      Bazz fizz! \n\
        \ "
    )
