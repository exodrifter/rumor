module Rumor.Parser.Let
( let'
) where

import Rumor.Parser.Common (Parser, hlexeme, lexeme, modifyVariableType, (<|>))

import qualified Rumor.Internal as Rumor
import qualified Rumor.Parser.Identifier as Identifier
import qualified Text.Megaparsec as Mega
import qualified Text.Megaparsec.Char as Char

-- $setup
-- >>> import Data.Either (fromRight)
-- >>> import Data.NonEmptyText as NET
-- >>> import Rumor.Parser.Common
-- >>> import Rumor.Internal
--
-- >>> let stringContext = fromRight newContext (setVariableType (VariableName (Unicode (NET.new 's' "tring"))) StringType newContext)
-- >>> let numberContext = fromRight newContext (setVariableType (VariableName (Unicode (NET.new 'n' "umber"))) NumberType newContext)
-- >>> let booleanContext = fromRight newContext (setVariableType (VariableName (Unicode (NET.new 'b' "oolean"))) BooleanType newContext)
-- >>> let parse c inner = parseTest c (inner <* eof)

{-| Parses a let binding, which can be used to assign types to variables.

  >>> parse newContext let' "let foo: Boolean"
  (VariableName (Unicode "foo"),BooleanType)
  Context {variableTypes = fromList [(VariableName (Unicode "foo"),BooleanType)]}

  >>> parse newContext let' "let foo: Number"
  (VariableName (Unicode "foo"),NumberType)
  Context {variableTypes = fromList [(VariableName (Unicode "foo"),NumberType)]}

  >>> parse newContext let' "let foo: String"
  (VariableName (Unicode "foo"),StringType)
  Context {variableTypes = fromList [(VariableName (Unicode "foo"),StringType)]}

  You cannot change the types of already-defined variables.

  >>> parse booleanContext let' "let boolean: Number"
  1:1:
    |
  1 | let boolean: Number
    | ^^^^^^^^^^^^^^^^^^^
  Variable `boolean` cannot be a Number; it has already been defined as a Boolean!

  >>> parse booleanContext let' "let boolean: String"
  1:1:
    |
  1 | let boolean: String
    | ^^^^^^^^^^^^^^^^^^^
  Variable `boolean` cannot be a String; it has already been defined as a Boolean!

  >>> parse numberContext let' "let number: Boolean"
  1:1:
    |
  1 | let number: Boolean
    | ^^^^^^^^^^^^^^^^^^^
  Variable `number` cannot be a Boolean; it has already been defined as a Number!

  >>> parse numberContext let' "let number: String"
  1:1:
    |
  1 | let number: String
    | ^^^^^^^^^^^^^^^^^^
  Variable `number` cannot be a String; it has already been defined as a Number!

  >>> parse stringContext let' "let string: Boolean"
  1:1:
    |
  1 | let string: Boolean
    | ^^^^^^^^^^^^^^^^^^^
  Variable `string` cannot be a Boolean; it has already been defined as a String!

  >>> parse stringContext let' "let string: Number"
  1:1:
    |
  1 | let string: Number
    | ^^^^^^^^^^^^^^^^^^
  Variable `string` cannot be a Number; it has already been defined as a String!

  You can re-define variables to the same type.

  >>> parse booleanContext let' "let boolean: Boolean"
  (VariableName (Unicode "boolean"),BooleanType)
  Context {variableTypes = fromList [(VariableName (Unicode "boolean"),BooleanType)]}

  >>> parse numberContext let' "let number: Number"
  (VariableName (Unicode "number"),NumberType)
  Context {variableTypes = fromList [(VariableName (Unicode "number"),NumberType)]}

  >>> parse stringContext let' "let string: String"
  (VariableName (Unicode "string"),StringType)
  Context {variableTypes = fromList [(VariableName (Unicode "string"),StringType)]}

  You can have extra horizontal whitespace around the colon, but not vertical
  whitespace.

  >>> parse newContext let' "let foobar:String"
  (VariableName (Unicode "foobar"),StringType)
  Context {variableTypes = fromList [(VariableName (Unicode "foobar"),StringType)]}

  >>> parse newContext let' "let foobar  :  String"
  (VariableName (Unicode "foobar"),StringType)
  Context {variableTypes = fromList [(VariableName (Unicode "foobar"),StringType)]}

  >>> parse newContext let' "let foobar\t:\tString"
  (VariableName (Unicode "foobar"),StringType)
  Context {variableTypes = fromList [(VariableName (Unicode "foobar"),StringType)]}

  >>> parse newContext let' "let foobar\n:\nString"
  1:11:
    |
  1 | let foobar
    |           ^
  unexpected newline
  expecting ':' or variable character
-}
let' :: Parser (Rumor.VariableName, Rumor.VariableType)
let' = do
  begin <- Mega.getOffset
  _ <- hlexeme "let"
  name <- hlexeme Identifier.variableName
  _ <- hlexeme (Char.char ':')
  typ <- Mega.try (do _ <- lexeme "Boolean"; pure Rumor.BooleanType)
     <|> Mega.try (do _ <- lexeme "Number"; pure Rumor.NumberType)
     <|> Mega.try (do _ <- lexeme "String"; pure Rumor.StringType)
  end <- Mega.getOffset

  modifyVariableType name typ begin end
  pure (name, typ)
