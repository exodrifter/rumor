module Rumor.Parser.Common
( Parser, runParser, parseTest
, Context, newContext
, rumorError

, lexeme, hlexeme
, space, hspace
, Mega.eof, eolf

-- Re-exports
, (<?>)
, (<|>)
) where

import Data.Text (Text)
import Text.Megaparsec ((<?>), (<|>))

import qualified Control.Monad.Reader as Reader
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Text.Megaparsec as Mega
import qualified Text.Megaparsec.Char as Char
import qualified Text.Megaparsec.Char.Lexer as Lexer
import qualified Text.Megaparsec.Error as Error
import qualified Rumor.Internal.Types as Rumor

-- $setup
-- >>> import qualified Text.Megaparsec as Mega
-- >>> let parse inner = parseTest newContext (inner <* Mega.eof)

type Parser a =
  Mega.ParsecT RumorError Text (Reader.Reader Context) a

runParser :: Context -> Parser a -> FilePath -> Text -> Either String a
runParser context parser fileName fileContents =
  let
    result =
      Reader.runReader
        (Mega.runParserT parser fileName fileContents)
        context
  in
    case result of
      Right a -> Right a
      Left err -> Left (Error.errorBundlePretty err)

parseTest :: Show a => Context -> Parser a -> Text -> IO ()
parseTest context parser text =
  case runParser context parser "" text of
    Left e -> putStr e
    Right x -> print x

newtype Context =
  Context
    { variableTypes :: Map.Map Rumor.Unicode Rumor.Type
    }

newContext :: Context
newContext =
  Context
    { variableTypes = Map.empty
    }

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

  >>> parse eolf ""
  ()

  >>> parse eolf "\r\n"
  ()

  >>> parse eolf "\n"
  ()

  >>> parse eolf "\r"
  ()
-}
eolf :: Parser ()
eolf =
      (do _ <- "\r\n"; pure ())
  <|> (do _ <- Char.char '\n'; pure ())
  <|> (do _ <- Char.char '\r'; pure ())
  <|> Mega.eof
