module Rumor.Node.SectionTest
( tests
) where

import Rumor.Expression.Type (Expression(..))
import Rumor.Node.Helper (runNodeParser, runNodesParser)
import Rumor.Node.Parser (nodes, section)
import Rumor.Node.Type (Node(..))

import Test.HUnit

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
    ( Right $ Section "foobar"
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
      [ Section "foo"
        [ Say Nothing (Text "Hello world! Hello everyone!")
        ]
      , Section "bar"
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
        Section "foo"
          [ Section "bar"
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
      [ Section "foo"
        [ Say Nothing (Text "Hello world! Hello everyone!")
        , Section "bar"
          [ Say Nothing (Text "Fizz bazz!")
          , Say Nothing (Text "Bazz fizz!")
          ]
        ]
      , Section "bizz"
        [ Append Nothing (Text "Hello world!")
        , Append Nothing (Text "Hello everyone!")
        , Section "bazz"
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
