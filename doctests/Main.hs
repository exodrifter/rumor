module Main where

import Test.DocTest (doctest)

main :: IO ()
main =
  doctest
    [ "-XBlockArguments"
    , "--fast"
    , "src"
    ]
