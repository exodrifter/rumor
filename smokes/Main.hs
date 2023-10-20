{-# LANGUAGE QuasiQuotes #-}
module Main where

import Text.Heredoc (str)

import qualified Rumor
import qualified Rumor.Internal.Types as Rumor
import qualified Test.HUnit as HUnit
import qualified Data.NonEmptyText as NET

main :: IO ()
main = HUnit.runTestTTAndExit tests

tests :: HUnit.Test
tests =
  HUnit.TestList
    [ smokeTest
    ]

smokeTest :: HUnit.Test
smokeTest =
  HUnit.TestCase do
    let mkDialog cons speaker line =
          cons (Just (Rumor.Speaker speaker)) (Rumor.String line) Nothing
        yuu = NET.new 'y' "uu"
        touko = NET.new 't' "ouko"

    HUnit.assertEqual "smoke test 1"
      ( Right
          [ mkDialog Rumor.Say yuu
              "Wah! Nanami-senpai!"
          , mkDialog Rumor.Say touko
              "Yuu!"
          , mkDialog Rumor.Add touko
              "Oh, thank you for finding those for us!"
          , mkDialog Rumor.Say yuu
              "No problem. Umm..."
          , mkDialog Rumor.Add yuu
              "Why are you coming in?"
          , mkDialog Rumor.Say touko
              "Hmm?"
          ]
      )
      ( Rumor.parse ""
          [str|// Bloom Into You, Chapter 15: On Your Mark
              |yuu: Wah! Nanami-senpai!
              |touko: Yuu!
              |touko+ Oh, thank you for finding those for us!
              |yuu: No problem. Umm...
              |yuu+ Why are you coming in?
              |touko: Hmm?
              |]
      )
