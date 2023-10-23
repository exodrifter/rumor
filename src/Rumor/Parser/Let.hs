module Rumor.Parser.Let
( let'
) where

import Rumor.Parser.Common (Parser, hlexeme, lexeme, rumorError, modifyVariableType, space, (<?>), (<|>))

import qualified Rumor.Internal.Types as Rumor
import qualified Rumor.Parser.Identifier as Identifier
import qualified Text.Megaparsec as Mega
import qualified Text.Megaparsec.Char as Char
import qualified Text.Megaparsec.Char.Lexer as Lexer

-- $setup
-- >>> import Data.Either (fromRight)
-- >>> import Data.NonEmptyText as NET
-- >>> import Rumor.Parser.Common
-- >>> import Rumor.Internal.Types
--
-- >>> :{
-- let setVariableTypes c0 = do
--       c1 <- setVariableType (VariableName (Unicode (NET.new 's' "tring"))) StringType c0
--       c2 <- setVariableType (VariableName (Unicode (NET.new 'n' "umber"))) NumberType c1
--       c3 <- setVariableType (VariableName (Unicode (NET.new 'b' "oolean"))) BooleanType c2
--       pure c3
-- :}
--
-- >>> let context = fromRight undefined (setVariableTypes newContext)
-- >>> let parse inner = parseTest context (inner <* eof)

{-| Parses a let binding, which can be used to assign types to variables.

  >>> parse let' "let foo: Boolean"
  (VariableName (Unicode "foo"),BooleanType)

  >>> parse let' "let foo: Number"
  (VariableName (Unicode "foo"),NumberType)

  >>> parse let' "let foo: String"
  (VariableName (Unicode "foo"),StringType)

  You cannot change the types of already-defined variables.

  >>> parse let' "let boolean: Number"
  1:1:
    |
  1 | let boolean: Number
    | ^^^^^^^^^^^^^^^^^^^
  Variable `boolean` cannot be a Number; it has already been defined as a Boolean!

  >>> parse let' "let boolean: String"
  1:1:
    |
  1 | let boolean: String
    | ^^^^^^^^^^^^^^^^^^^
  Variable `boolean` cannot be a String; it has already been defined as a Boolean!

  >>> parse let' "let number: Boolean"
  1:1:
    |
  1 | let number: Boolean
    | ^^^^^^^^^^^^^^^^^^^
  Variable `number` cannot be a Boolean; it has already been defined as a Number!

  >>> parse let' "let number: String"
  1:1:
    |
  1 | let number: String
    | ^^^^^^^^^^^^^^^^^^
  Variable `number` cannot be a String; it has already been defined as a Number!

  >>> parse let' "let string: Boolean"
  1:1:
    |
  1 | let string: Boolean
    | ^^^^^^^^^^^^^^^^^^^
  Variable `string` cannot be a Boolean; it has already been defined as a String!

  >>> parse let' "let string: Number"
  1:1:
    |
  1 | let string: Number
    | ^^^^^^^^^^^^^^^^^^
  Variable `string` cannot be a Number; it has already been defined as a String!

  You can re-define variables to the same type.

  >>> parse let' "let boolean: Boolean"
  (VariableName (Unicode "boolean"),BooleanType)

  >>> parse let' "let number: Number"
  (VariableName (Unicode "number"),NumberType)

  >>> parse let' "let string: String"
  (VariableName (Unicode "string"),StringType)

  You can have extra horizontal whitespace around the colon, but not vertical
  whitespace.

  >>> parse let' "let foobar:String"
  (VariableName (Unicode "foobar"),StringType)

  >>> parse let' "let foobar  :  String"
  (VariableName (Unicode "foobar"),StringType)

  >>> parse let' "let foobar\t:\tString"
  (VariableName (Unicode "foobar"),StringType)

  >>> parse let' "let foobar\n:\nString"
  1:11:
    |
  1 | let foobar
    |           ^
  unexpected newline
  expecting ':'
-}
let' :: Parser (Rumor.VariableName, Rumor.Type)
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

          result <- modifyVariableType name typ
          case result of
            Left err ->
              pure (Left err, end - start)
            Right () ->
              pure (Right (name, typ), end - start)

  result <- Mega.lookAhead parser <?> "variable"
  case result of
    (Left err, len) ->
      rumorError err len
    (Right r, len) -> do
      _ <- Mega.takeP Nothing len
      pure r
