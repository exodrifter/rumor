module Rumor.Parser.Common
( Parser, runParser, parseTest

, Context, newContext
, setVariableType

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

import qualified Control.Monad.State.Strict as State
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

--------------------------------------------------------------------------------
-- Parser
--------------------------------------------------------------------------------

type Parser a =
  Mega.ParsecT RumorError Text (State.State Context) a

runParser :: Context -> Parser a -> FilePath -> Text -> Either String a
runParser context parser fileName fileContents =
  let
    result =
      State.runState
        (Mega.runParserT parser fileName fileContents)
        context
  in
    case result of
      (Right a, _) -> Right a
      (Left err, _) -> Left (Error.errorBundlePretty err)

parseTest :: Show a => Context -> Parser a -> Text -> IO ()
parseTest context parser text =
  case runParser context parser "" text of
    Left e -> putStr e
    Right x -> print x

--------------------------------------------------------------------------------
-- Typing
--------------------------------------------------------------------------------

newtype Context =
  Context
    { variableTypes :: Map.Map Rumor.VariableName Rumor.Type
    }

newContext :: Context
newContext =
  Context
    { variableTypes = Map.empty
    }

setVariableType :: Rumor.VariableName -> Rumor.Type -> Parser (Either Text ())
setVariableType name typ = do
  context <- State.get
  case Map.lookup name (variableTypes context) of
    Just _ ->
      pure (Left (Rumor.variableNameToText name <> " has already been defined!"))
    Nothing -> do
      State.put
        Context { variableTypes = Map.insert name typ (variableTypes context) }
      pure (Right ())

--------------------------------------------------------------------------------
-- Errors
--------------------------------------------------------------------------------

data RumorError = RumorError { rumorErrorToText :: Text, rumorErrorLength :: Int }
  deriving (Eq, Ord)

instance Error.ShowErrorComponent RumorError where
  showErrorComponent = T.unpack . rumorErrorToText
  errorComponentLen = rumorErrorLength

rumorError :: Text -> Int -> Parser a
rumorError message len = Mega.customFailure (RumorError message len)

--------------------------------------------------------------------------------
-- Whitespace
--------------------------------------------------------------------------------

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
