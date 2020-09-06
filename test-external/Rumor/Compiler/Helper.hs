module Rumor.Compiler.Helper
( parse
, scriptSingleton
) where

import Rumor (Node, ParseError, Script(..))
import qualified Rumor

import Data.Fixed (E12)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T

parse :: T.Text -> Either ParseError (Script E12)
parse = Rumor.parse ""

scriptSingleton :: Node r -> Script r
scriptSingleton n = Script Map.empty [n]
