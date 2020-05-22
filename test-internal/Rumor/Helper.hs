module Rumor.Helper
( runTestParser
) where

import Prelude (Either(..))
import Text.Parsec (ParseError, runParser)
import Text.Parsec.Text (Parser)
import qualified Data.Text as T

runTestParser :: Parser a -> T.Text -> Either ParseError a
runTestParser parser input = runParser parser () "" input
