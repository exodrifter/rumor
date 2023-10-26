module Rumor
( parse
, Rumor.newContext
) where

import Rumor.Parser (nodes)

import qualified Data.Text as T
import qualified Rumor.Parser.Common as Parser
import qualified Rumor.Internal as Rumor

parse :: Rumor.Context -> String -> T.Text -> Either String [Rumor.Node]
parse context = Parser.runParser context nodes
