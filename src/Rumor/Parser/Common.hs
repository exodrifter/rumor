module Rumor.Parser.Common
( Parser
, rumorError

, lexeme, hlexeme
, space, hspace
, eolf

-- Re-exports
, (<?>)
, (<|>)
) where

import Data.Text (Text)
import Text.Megaparsec ((<?>), (<|>))

import qualified Data.Text as T
import qualified Text.Megaparsec as Mega
import qualified Text.Megaparsec.Char as Char
import qualified Text.Megaparsec.Char.Lexer as Lexer
import qualified Text.Megaparsec.Error as Error

-- $setup
-- >>> import qualified Text.Megaparsec as Mega
-- >>> let parseTest inner = Mega.parseTest (inner <* Mega.eof)

type Parser a = Mega.Parsec RumorError Text a

data RumorError = RumorError { rumorErrorToText :: Text, rumorErrorLength :: Int }
  deriving (Eq, Ord)

instance Error.ShowErrorComponent RumorError where
  showErrorComponent = T.unpack . rumorErrorToText
  errorComponentLen = rumorErrorLength

rumorError :: Text -> Int -> Parser a
rumorError message len = Mega.customFailure (RumorError message len)

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
