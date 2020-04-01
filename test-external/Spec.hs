import Prelude (IO, pure)
import Test.HUnit (Test(..), runTestTT)
import qualified Rumor.ComplexEvaluateTest
import qualified Rumor.ComplexSimplifyTest

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
