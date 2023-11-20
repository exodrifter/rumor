module Main where

import qualified Rumor
import qualified Rumor.Internal as Internal
import qualified Data.Text.IO as TIO
import qualified System.FilePath as FilePath
import qualified Test.HUnit as HUnit

main :: IO ()
main = HUnit.runTestTTAndExit tests

tests :: HUnit.Test
tests =
  let
    smoke fileName =
      HUnit.TestCase do
        contents <- TIO.readFile fileName

        let result = Rumor.parse Rumor.newContext fileName contents
        case result of
          Right (nodes, _context) -> do
            TIO.writeFile
              (FilePath.replaceExtension fileName "golden")
              (Internal.nodesToDebugText nodes)
            pure ()
          Left err ->
            HUnit.assertString err
  in
    HUnit.TestList
      [ smoke "smokes/examples/bloom-into-you.rumor"
      , smoke "smokes/examples/gender-dysphoria.rumor"
      ]
