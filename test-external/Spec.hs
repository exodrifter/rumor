import qualified Rumor.ComplexEvaluateTest
import qualified Rumor.ComplexSimplifyTest

import Test.HUnit (Test(..), runTestTT)

main :: IO ()
main = do
  _ <- runTestTT tests
  pure ()

tests :: Test
tests =
  TestList
    [ Rumor.ComplexEvaluateTest.tests
    , Rumor.ComplexSimplifyTest.tests
    ]
