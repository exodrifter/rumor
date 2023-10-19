module Rumor.Parser.Common
( Parser
, lexeme, hlexeme
, space, hspace

-- Re-exports
, (<?>)
, (<|>)
) where

import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec ((<?>), (<|>))

import qualified Text.Megaparsec as Mega
import qualified Text.Megaparsec.Char as Char
import qualified Text.Megaparsec.Char.Lexer as Lexer

type Parser a = Mega.Parsec Void Text a

lexeme :: Parser a -> Parser a
lexeme = Lexer.lexeme space

hlexeme :: Parser a -> Parser a
hlexeme = Lexer.lexeme hspace

space :: Parser ()
space = Lexer.space Char.space1 lineComment blockComment

hspace :: Parser ()
hspace = Lexer.space Char.hspace1 lineComment blockComment

lineComment :: Parser ()
lineComment = Lexer.skipLineComment "//"

blockComment :: Parser ()
blockComment = Lexer.skipBlockComment "/*" "*/"
