module Rumor.Parser.Common
( Parser
, lexeme, hlexeme
, space, hspace
, eolf

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

-- $setup
-- >>> import qualified Text.Megaparsec as Mega
-- >>> let parseTest inner = Mega.parseTest (inner <* Mega.eof)

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

{-| Parses a newline or the end of the file.

  >>> parseTest eolf ""
  ()

  >>> parseTest eolf "\r\n"
  ()

  >>> parseTest eolf "\n"
  ()

  >>> parseTest eolf "\r"
  ()
-}
eolf :: Parser ()
eolf =
      (do _ <- "\r\n"; pure ())
  <|> (do _ <- Char.char '\n'; pure ())
  <|> (do _ <- Char.char '\r'; pure ())
  <|> Mega.eof
