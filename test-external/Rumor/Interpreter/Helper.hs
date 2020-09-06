module Rumor.Interpreter.Helper
( compile
) where

import qualified Rumor

import Data.Fixed (E12)
import qualified Data.Text as T

compile :: T.Text -> Rumor.Context E12
compile = Rumor.init . either (error . show) id . Rumor.parse ""
