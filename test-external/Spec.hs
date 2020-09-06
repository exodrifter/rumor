import qualified Rumor.Compiler.DialogParserTest
import qualified Rumor.Compiler.NodesParserTest
import qualified Rumor.Compiler.SectionParserTest
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
    [ TestList  -- Expression Tests
      [ Rumor.ComplexEvaluateTest.tests
      , Rumor.ComplexSimplifyTest.tests
      ]
    , TestList  -- Interpreter Tests
      [ Rumor.Interpreter.RunTest.tests
      ]
    , TestList -- Compiler Tests
      [ Rumor.Compiler.DialogParserTest.tests
      , Rumor.Compiler.NodesParserTest.tests
      , Rumor.Compiler.SectionParserTest.tests
      ]
    ]
