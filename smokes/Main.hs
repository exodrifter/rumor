module Main where

import qualified Rumor
import qualified Data.Text.IO as TIO
import qualified Test.HUnit as HUnit

main :: IO ()
main = HUnit.runTestTTAndExit tests

tests :: HUnit.Test
tests =
  let
    smoke fileName =
      HUnit.TestCase do
        contents <- TIO.readFile fileName

        let result = Rumor.parse fileName contents
        case result of
          Right _ ->
            pure ()
          Left err ->
            HUnit.assertString err
  in
    HUnit.TestList
      [ smoke "smokes/examples/bloom-into-you.rumor"
      , smoke "smokes/examples/gender-dysphoria.rumor"
      ]
