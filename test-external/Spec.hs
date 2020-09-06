import qualified Rumor.ComplexEvaluateTest
import qualified Rumor.ComplexSimplifyTest
import qualified Rumor.Interpreter.RunTest

import Test.HUnit (Test(..), runTestTT)

main :: IO ()
main = do
  _ <- runTestTT tests
  pure ()

tests :: Test
tests =
  TestList
    [ TestList
      [ Rumor.ComplexEvaluateTest.tests
      , Rumor.ComplexSimplifyTest.tests
      ]
    , TestList
      [ Rumor.Interpreter.RunTest.tests
      ]
    ]
