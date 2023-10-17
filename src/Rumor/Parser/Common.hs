module Rumor.Parser.Common
( Parser
) where

import Data.Text (Text)
import Data.Void (Void)

import qualified Text.Megaparsec as Mega

type Parser a = Mega.Parsec Void Text a
