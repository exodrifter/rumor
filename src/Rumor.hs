module Rumor where

import Rumor.Parser (nodes)

import qualified Data.Text as T
import qualified Text.Megaparsec as Mega
import qualified Text.Megaparsec.Error as Error
import qualified Rumor.Internal.Types as Rumor

parse :: String -> T.Text -> Either String [Rumor.Node]
parse fileName fileContents =
  case Mega.runParser nodes fileName fileContents of
    Right result -> Right result
    Left err -> Left (Error.errorBundlePretty err)
