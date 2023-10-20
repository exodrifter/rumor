module Rumor
( parse
, Parser.newContext
) where

import Rumor.Parser (nodes)

import qualified Data.Text as T
import qualified Rumor.Parser.Common as Parser
import qualified Rumor.Internal.Types as Rumor

parse :: Parser.Context -> String -> T.Text -> Either String [Rumor.Node]
parse context = Parser.runParser context nodes
