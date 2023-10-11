module Rumor where

import Data.Text (Text)
import Data.Void (Void)

import qualified Data.Char as Char
import qualified Data.Maybe as Maybe
import qualified Data.Text as T
import qualified Text.Megaparsec as Mega
import qualified Text.Megaparsec.Char as Char
import qualified Text.Megaparsec.Char.Lexer as Lexer
import qualified Text.Megaparsec.Error as Error
import qualified Rumor.Internal.Types as Rumor

type Parser a = Mega.Parsec Void Text a

parse :: String -> T.Text -> Either String [Rumor.Node]
parse fileName fileContents =
  case Mega.runParser parser fileName fileContents of
    Right result -> Right result
    Left err -> Left (Error.errorBundlePretty err)

parser :: Parser [Rumor.Node]
parser =
  Mega.many (lexeme node <* space) <* Mega.eof

node :: Parser Rumor.Node
node =
  Mega.choice
    [ Mega.try say
    , add
    ]

say :: Parser Rumor.Node
say =
  lexeme (dialog ':' Rumor.Say)

add :: Parser Rumor.Node
add =
  lexeme (dialog '+' Rumor.Add)

dialog ::
  Char -> (Maybe Rumor.Speaker -> Text -> Rumor.Node) -> Parser Rumor.Node
dialog sep cons =
  let
    mkDialog speaker texts =
      cons (Rumor.Speaker <$> speaker) (T.strip (T.intercalate " " texts))

    dialogIndent = do
      speaker <- lexeme (Mega.optional identifier)
      _ <- lexeme (Char.char sep)
      firstText <- Maybe.fromMaybe "" <$> (Mega.optional line)
      pure
        ( Lexer.IndentMany
            Nothing
            (\texts -> pure (mkDialog speaker (firstText:texts)))
            line
        )

  in
    Lexer.indentBlock space dialogIndent

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

identifier :: Parser Text
identifier =
  Mega.takeWhile1P
    (Just "letter, mark, or digit character")
    (\char -> Char.isLetter char || Char.isMark char || Char.isDigit char)

line :: Parser Text
line =
  Mega.takeWhile1P Nothing (`notElem` ['\n', '\r'])

lexeme :: Parser a -> Parser a
lexeme = Lexer.lexeme hspace

space :: Parser ()
space = Lexer.space Char.space1 lineComment blockComment

hspace :: Parser ()
hspace = Lexer.space Char.hspace1 lineComment blockComment

lineComment :: Parser ()
lineComment = Lexer.skipLineComment "//"

blockComment :: Parser ()
blockComment = Lexer.skipBlockComment "/*" "*/"
