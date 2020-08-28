import qualified Rumor.Expression.BooleanParserTest
import qualified Rumor.Expression.EvaluateTest
import qualified Rumor.Expression.NumberParserTest
import qualified Rumor.Expression.SimplifyTest
import qualified Rumor.Expression.TextParserTest

import Test.HUnit (Test(..), runTestTT)

main :: IO ()
main = do
  _ <- runTestTT tests
  pure ()

tests :: Test
tests =
  TestList
    -- Expression type tests
    [ Rumor.Expression.EvaluateTest.tests
    , Rumor.Expression.SimplifyTest.tests

    -- Parsing tests
    , Rumor.Expression.BooleanParserTest.tests
    , Rumor.Expression.NumberParserTest.tests
    , Rumor.Expression.TextParserTest.tests
    ]
