import qualified Rumor.Expression.BooleanParserTest
import qualified Rumor.Expression.EvaluateTest
import qualified Rumor.Expression.NumberParserTest
import qualified Rumor.Expression.SimplifyTest
import qualified Rumor.Expression.TextParserTest
import qualified Rumor.Node.DialogParserTest
import qualified Rumor.Node.NodesParserTest

import Test.HUnit (Test(..), runTestTT)

main :: IO ()
main = do
  _ <- runTestTT tests
  pure ()

tests :: Test
tests =
  TestList
    [ TestList -- Expression Tests
      [ Rumor.Expression.BooleanParserTest.tests
      , Rumor.Expression.EvaluateTest.tests
      , Rumor.Expression.NumberParserTest.tests
      , Rumor.Expression.SimplifyTest.tests
      , Rumor.Expression.TextParserTest.tests
      ]
    , TestList -- Node Tests
      [ Rumor.Node.DialogParserTest.tests
      , Rumor.Node.NodesParserTest.tests
      ]
    ]
