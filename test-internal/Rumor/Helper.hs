module Rumor.Helper
( runTestParser
) where

import Rumor.Prelude
import Rumor.Parser
import qualified Data.Text as T

runTestParser :: Parser a -> T.Text -> Either ParseError a
runTestParser parser = runParser parser ""
