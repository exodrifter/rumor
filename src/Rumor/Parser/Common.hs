module Rumor.Parser.Common
( Parser, runParser, parseTest
, State.gets

, Rumor.Context, Rumor.newContext
, Rumor.getVariableType
, Rumor.setVariableType, modifyVariableType

, RumorError(..)
, rumorError
, inferenceError

, lexeme, hlexeme
, space, hspace
, Mega.eof, Char.eol, eolf

-- Re-exports
, (<?>)
, (<|>)
) where

import Data.Text (Text)
import Text.Megaparsec ((<?>), (<|>))

import qualified Control.Monad.State.Strict as State
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Text.Megaparsec as Mega
import qualified Text.Megaparsec.Char as Char
import qualified Text.Megaparsec.Char.Lexer as Lexer
import qualified Text.Megaparsec.Error as Error
import qualified Rumor.Internal as Rumor

-- $setup
-- >>> import qualified Text.Megaparsec as Mega
-- >>> let parse inner = parseTest Rumor.newContext (inner <* Mega.eof)

--------------------------------------------------------------------------------
-- Parser
--------------------------------------------------------------------------------

type Parser a =
  Mega.ParsecT RumorError Text (State.State Rumor.Context) a

runParser :: Rumor.Context -> Parser a -> FilePath -> Text -> Either String a
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

parseTest :: Show a => Rumor.Context -> Parser a -> Text -> IO ()
parseTest context parser text =
  case runParser context parser "" text of
    Left e -> putStr e
    Right x -> print x

--------------------------------------------------------------------------------
-- Typing
--------------------------------------------------------------------------------

modifyVariableType :: Rumor.VariableName -> Rumor.VariableType -> Int -> Int -> Parser ()
modifyVariableType name typ pos len = do
  oldContext <- State.get
  case Rumor.setVariableType name typ oldContext of
    Left err ->
      rumorError err pos len
    Right context -> do
      State.put context
      pure ()

--------------------------------------------------------------------------------
-- Errors
--------------------------------------------------------------------------------

data RumorError =
    RumorError Text Int
  | CannotInferVariable Rumor.VariableName
  | CannotInferExpression
  | TypeMismatch Rumor.VariableType Rumor.VariableType
  | UnexpectedFailure
  deriving (Eq, Ord, Show)

instance Error.ShowErrorComponent RumorError where
  showErrorComponent err =
    case err of
      RumorError msg _ ->
        T.unpack msg
      CannotInferVariable name ->
           "Cannot infer type of `"
        <> T.unpack (Rumor.variableNameToText name)
        <> "`."
      CannotInferExpression ->
        "Cannot infer the type of this expression."
      TypeMismatch expected actual ->
           "Expected a "
        <> T.unpack (Rumor.typeToText expected)
        <> " expression but this expression is actually a "
        <> T.unpack (Rumor.typeToText actual)
        <> "!"
      UnexpectedFailure ->
        "Unexpected error when inferring expression type! \
        \This is a bug; please report this."
  errorComponentLen err = -- TODO: Better error positions
    case err of
      RumorError _ len -> len
      CannotInferVariable _ -> 0
      CannotInferExpression -> 0
      TypeMismatch _ _ -> 0
      UnexpectedFailure -> 0

rumorError :: Text -> Int -> Int -> Parser a
rumorError message pos len =
  let
    err = Mega.ErrorCustom (RumorError message len)
  in
    Mega.parseError (Mega.FancyError pos (Set.singleton err))

-- TODO: Replace rumorError with this
inferenceError :: RumorError -> Parser a
inferenceError customError = do
  let
    err = Mega.ErrorCustom customError

  pos <- Mega.getOffset
  Mega.parseError (Mega.FancyError pos (Set.singleton err))

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
  1:1:
    |
  1 |
    | ^
  unexpected carriage return
  expecting end of input or end of line
-}
eolf :: Parser ()
eolf =
      (do _ <- Char.eol; pure ())
  <|> Mega.eof
