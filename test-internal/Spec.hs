import qualified Rumor.Compiler.BooleanParserTest
import qualified Rumor.Compiler.NumberParserTest
import qualified Rumor.Compiler.TextParserTest
import qualified Rumor.Expression.EvaluateTest
import qualified Rumor.Expression.SimplifyTest

import Test.HUnit (Test(..), runTestTT)

main :: IO ()
main = do
  _ <- runTestTT tests
  pure ()

tests :: Test
tests =
  TestList
    [ TestList -- Expression Tests
      [ Rumor.Expression.EvaluateTest.tests
      , Rumor.Expression.SimplifyTest.tests
      ]
    , TestList -- Compiler Tests
      [ Rumor.Compiler.BooleanParserTest.tests
      , Rumor.Compiler.NumberParserTest.tests
      , Rumor.Compiler.TextParserTest.tests
      ]
    ]
