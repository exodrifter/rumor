module Rumor.Parser.Identifier
( identifier
) where

import Data.Char (isLetter, isMark, isDigit)
import Data.Text (Text)
import Rumor.Parser.Common (Parser)

import qualified Text.Megaparsec as Mega

identifier :: Parser Text
identifier =
  Mega.takeWhile1P
    (Just "letter, mark, or digit character")
    (\ch -> isLetter ch || isMark ch || isDigit ch)
