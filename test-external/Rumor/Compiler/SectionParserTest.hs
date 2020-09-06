module Rumor.Compiler.SectionParserTest
( tests
) where

import Rumor.Compiler.Helper (parse)
import Rumor (Expression(..), Node(..), Script(..))

import Test.HUnit
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map

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
    ( Right Script
      { sections = Map.fromList
        [ ("foobar", NE.fromList
            [ Say Nothing (Text "Hello world!")
            , Say Nothing (Text "Fizz bazz!")
            ]
          )
        ]
      , nodes = []
      }
    )
    ( parse
        "label [foobar] \n\
        \  : Hello world! \n\
        \  : Fizz bazz! \n\
        \ "
    )

multiSectionTest :: Test
multiSectionTest =
  TestCase $ assertEqual "Parses multiple sections"
    ( Right Script
      { sections = Map.fromList
        [ ("foo", NE.fromList
            [ Say Nothing (Text "Hello world! Hello everyone!")
            ]
          )
        , ("bar", NE.fromList
            [ Say Nothing (Text "Fizz bazz!")
            , Say Nothing (Text "Bazz fizz!")
            ]
          )
        ]
      , nodes = []
      }
    )
    ( parse
        "label [foo] \n\
        \  : Hello world! \n\
        \    Hello everyone! \n\
        \label [bar] \n\
        \  : Fizz bazz! \n\
        \  : Bazz fizz! \n\
        \ "
    )

nestedSectionTest :: Test
nestedSectionTest =
  TestCase $ assertEqual "Parses a nested section"
    ( Right Script
      { sections = Map.fromList
        [ ("foo", NE.fromList
            [ Say Nothing (Text "Hi there!")
            ]
          )
        , ("bar", NE.fromList
            [ Say Nothing (Text "Hello world!")
            ]
          )
        ]
      , nodes = []
      }
    )
    ( parse
        "label [foo] \n\
        \  : Hi there! \n\
        \  label [bar] \n\
        \    : Hello world! \n\
        \ "
    )

multiNestedSectionTest :: Test
multiNestedSectionTest =
  TestCase $ assertEqual "Parses multiple nested sections"
    ( Right Script
      { sections = Map.fromList
        [ ("foo", NE.fromList
            [ Say Nothing (Text "Hello world! Hello everyone!")
            ]
          )
        , ("bar", NE.fromList
            [ Say Nothing (Text "Fizz bazz!")
            , Say Nothing (Text "Bazz fizz!")
            ]
          )
        , ("bizz", NE.fromList
            [ Append Nothing (Text "Hello world!")
            , Append Nothing (Text "Hello everyone!")
            ]
          )
        , ("bazz", NE.fromList
            [ Append Nothing (Text "Fizz bazz! Bazz fizz!")
            ]
          )
        ]
      , nodes = []
      }
    )
    ( parse
        "label [foo] \n\
        \  : Hello world! \n\
        \    Hello everyone! \n\
        \  label [bar] \n\
        \    : Fizz bazz! \n\
        \    : Bazz fizz! \n\
        \label [bizz] \n\
        \  + Hello world! \n\
        \  + Hello everyone! \n\
        \  label [bazz] \n\
        \    + Fizz bazz! \n\
        \      Bazz fizz! \n\
        \ "
    )
