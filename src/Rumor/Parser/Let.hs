module Rumor.Parser.Let
( let'
) where

import Rumor.Parser.Common (Parser, hlexeme, lexeme, rumorError, setVariableType, space, (<?>), (<|>))

import qualified Rumor.Internal.Types as Rumor
import qualified Rumor.Parser.Identifier as Identifier
import qualified Text.Megaparsec as Mega
import qualified Text.Megaparsec.Char as Char
import qualified Text.Megaparsec.Char.Lexer as Lexer

-- $setup
-- >>> import Rumor.Parser.Common
-- >>> let parse inner = parseTest newContext (inner <* eof)

let' :: Parser ()
let' = do
  let parser = do
        start <- Mega.getOffset
        Lexer.nonIndented space do
          _ <- hlexeme "let"
          name <- hlexeme Identifier.variableName
          _ <- hlexeme (Char.char ':')
          typ <- Mega.try (do _ <- lexeme "Boolean"; pure Rumor.BooleanType)
             <|> Mega.try (do _ <- lexeme "Number"; pure Rumor.NumberType)
             <|> Mega.try (do _ <- lexeme "String"; pure Rumor.StringType)
          end <- Mega.getOffset

          result <- setVariableType name typ
          pure (result, end - start)

  result <- Mega.lookAhead parser <?> "variable"
  case result of
    (Left err, len) ->
      rumorError err len
    (Right (), len) -> do
      _ <- Mega.takeP Nothing len
      pure ()
