module Rumor.Parser.Common
( Parser, runParser, parseTest
, State.gets

, Rumor.Context, Rumor.newContext
, Rumor.getVariableType
, Rumor.setVariableType
, modifyVariableType

, RumorError(..)
, throw

, lexeme, hlexeme, hlexeme1
, space, hspace, hspace1
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

runParser ::
  Rumor.Context ->
  Parser a ->
  FilePath ->
  Text ->
  Either String (a, Rumor.Context)
runParser context parser fileName fileContents =
  let
    result =
      State.runState
        (Mega.runParserT parser fileName fileContents)
        context
  in
    case result of
      (Right a, updatedContext) -> Right (a, updatedContext)
      (Left err, _) -> Left (Error.errorBundlePretty err)

parseTest :: Show a => Rumor.Context -> Parser a -> Text -> IO ()
parseTest context parser text =
  case runParser context parser "" text of
    Left e ->
      putStr e
    Right (a, updatedContext) -> do
      print a
      State.when (updatedContext /= Rumor.newContext) do
        print updatedContext

--------------------------------------------------------------------------------
-- Typing
--------------------------------------------------------------------------------

modifyVariableType ::
  Rumor.VariableName -> Rumor.VariableType -> Int -> Int -> Parser ()
modifyVariableType name new begin end = do
  oldContext <- State.get
  case Rumor.setVariableType name new oldContext of
    Left old ->
      throw (VariableAlreadyDefinedAs name old new begin end)
    Right context -> do
      State.put context
      pure ()

--------------------------------------------------------------------------------
-- Errors
--------------------------------------------------------------------------------

data RumorError =
    CannotInferVariable Rumor.AnnotatedExpression Rumor.VariableName
  | CannotUseReservedKeyword Rumor.VariableName Int Int
  | TypeMismatch Rumor.AnnotatedExpression Rumor.VariableType
  | VariableAlreadyDefinedAs Rumor.VariableName Rumor.VariableType Rumor.VariableType Int Int
  deriving (Eq, Ord, Show)

instance Error.ShowErrorComponent RumorError where
  showErrorComponent err =
    case err of
      CannotInferVariable _ name ->
           "Cannot infer type of the variable `"
        <> T.unpack (Rumor.variableNameToText name)
        <> "`."
      CannotUseReservedKeyword name _ _ ->
          "Cannot use `"
        <> T.unpack (Rumor.variableNameToText name)
        <> "` as a variable name because it is a reserved keyword"
      TypeMismatch _ actual ->
           "Expected expression to have the type "
        <> T.unpack (Rumor.typeToText actual)
        <> "!"
      VariableAlreadyDefinedAs name old new _ _ ->
           "Variable `"
        <> T.unpack (Rumor.variableNameToText name)
        <> "` cannot be a "
        <> T.unpack (Rumor.typeToText new)
        <> "; it has already been defined as a "
        <> T.unpack (Rumor.typeToText old)
        <> "!"
  errorComponentLen err = -- TODO: Better error positions
    case err of
      CannotInferVariable annotation _ -> Rumor.annotationLength annotation
      CannotUseReservedKeyword _ begin end -> end - begin
      TypeMismatch annotation _ -> Rumor.annotationLength annotation
      VariableAlreadyDefinedAs _ _ _ begin end -> end - begin

throw :: RumorError -> Parser a
throw customError = do
  let
    pos =
      case customError of
        CannotUseReservedKeyword _ begin _ -> begin
        CannotInferVariable annotation _ -> Rumor.annotationBegin annotation
        TypeMismatch annotation _ -> Rumor.annotationBegin annotation
        VariableAlreadyDefinedAs _ _ _ begin _ -> begin
  let
    err = Mega.ErrorCustom customError

  Mega.parseError (Mega.FancyError pos (Set.singleton err))

--------------------------------------------------------------------------------
-- Whitespace
--------------------------------------------------------------------------------

lexeme :: Parser a -> Parser a
lexeme = Lexer.lexeme space

hlexeme :: Parser a -> Parser a
hlexeme = Lexer.lexeme hspace

hlexeme1 :: Parser a -> Parser a
hlexeme1 = Lexer.lexeme hspace1

space :: Parser ()
space = Lexer.space Char.space1 lineComment blockComment

hspace :: Parser ()
hspace = Lexer.space Char.hspace1 lineComment blockComment

hspace1 :: Parser ()
hspace1 =
      Mega.try (blockComment *> Char.hspace1)
  <|> Mega.try (Char.hspace1 *> blockComment)
  <|> Mega.try Char.hspace1

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
