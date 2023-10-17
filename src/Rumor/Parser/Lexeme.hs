module Rumor.Parser.Lexeme
( lexeme
, hlexeme
, space
, hspace
) where

import Rumor.Parser.Common (Parser)

import qualified Text.Megaparsec.Char as Char
import qualified Text.Megaparsec.Char.Lexer as Lexer

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
