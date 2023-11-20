module Main where

import Data.Text (Text)

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Rumor
import qualified Rumor.Internal as Rumor
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
          Right a -> do
            TIO.writeFile
              (FilePath.replaceExtension fileName "golden")
              (toDebugText a)
            pure ()
          Left err -> do
            TIO.writeFile
              (FilePath.replaceExtension fileName "golden")
              (T.pack err)
            HUnit.assertString err
  in
    HUnit.TestList
      [ smoke "smokes/examples/bloom-into-you.rumor"
      , smoke "smokes/examples/gender-dysphoria.rumor"
      ]

toDebugText :: ([Rumor.Node], Rumor.Context) -> Text
toDebugText (nodes, context) =
     Rumor.nodesToDebugText nodes
  <> "\n\n===\n\n"
  <> Rumor.contextToDebugText context
