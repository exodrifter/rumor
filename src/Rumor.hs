module Rumor where

import Data.Functor (($>))
import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec ((<?>))

import qualified Data.Char as Char
import qualified Data.List as List
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
  Mega.manyTill (hlexeme node <* space) Mega.eof

node :: Parser Rumor.Node
node =
  Mega.choice
    [ Mega.try say
    , add
    ]

say :: Parser Rumor.Node
say =
  hlexeme (dialog ':' Rumor.Say)

add :: Parser Rumor.Node
add =
  hlexeme (dialog '+' Rumor.Add)

dialog ::
  Char ->
  (Maybe Rumor.Speaker -> Rumor.Expression Text -> Rumor.Node) ->
  Parser Rumor.Node
dialog sep cons =
  let
    mkDialog speaker texts =
      cons
        (Rumor.Speaker <$> speaker)
        ( mconcat
            ( List.intersperse
                (Rumor.String " ")
                (List.filter (/= mempty) texts)
            )
        )

    dialogIndent = do
      speaker <- hlexeme (Mega.optional identifier)
      _ <- hlexeme (Char.char sep)
      firstText <- unquotedLine
      pure
        ( Lexer.IndentMany
            Nothing
            (\texts -> pure (mkDialog speaker (firstText:texts)))
            unquotedLine
        )

  in
    Lexer.indentBlock space dialogIndent

--------------------------------------------------------------------------------
-- Expression
--------------------------------------------------------------------------------

interpolation :: Parser (Rumor.Expression Text)
interpolation = do
  _ <- lexeme (Char.char '{')
  text <- lexeme textExpression
  _ <- Char.char '}' <?> "end interpolation"
  pure text

textExpression :: Parser (Rumor.Expression Text)
textExpression = do
  _ <- Char.char '"'
  text <- unquotedLine
  _ <- Char.char '"' <?> "end double quote"
  pure text

unquotedLine :: Parser (Rumor.Expression Text)
unquotedLine = do
  let escapedChar = do
        _ <- Char.char '\\'
        Rumor.String <$>
          Mega.choice
            [ Char.char 'n' $> "\n"
            , Char.char 'r' $> "\r"
            , Char.char '\\' $> "\\"
            , Char.char '{' $> "{"
            , Char.char '}' $> "}"
            , Char.char '"' $> "\""
            ]
      literalString = do
        literal <-
          Mega.takeWhile1P
            (Just "literal char")
            (`notElem` ['\n', '\r', '\\', '{', '"'])
            :: Parser Text

        -- Strip the end of the text if this is at the end of the line
        next <- Mega.lookAhead
          (Mega.optional (Mega.choice [Char.char '\n', Char.char '\r']))
        if next == Just '\n' || next == Just '\r'
        then pure (Rumor.String (T.stripEnd literal))
        else pure (Rumor.String literal)

  text <-
      ( Mega.many
        ( Mega.choice
          [ literalString
          , escapedChar
          , interpolation
          ]
        )
      )
  pure (mconcat text)

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
