module Rumor.Parser.Set
( set
) where

import Rumor.Parser.Common (Parser, hlexeme, modifyVariableType, (<|>))

import qualified Rumor.Internal as Rumor
import qualified Rumor.Parser.Identifier as Identifier
import qualified Rumor.Parser.Expression as Expression
import qualified Rumor.Parser.Surround as Surround
import qualified Rumor.TypeCheck as TypeCheck
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

{-| Parses an set node, which can be used to assign values to variables.

  >>> parse newContext set "foo = true"
  Set (VariableName (Unicode "foo")) (Boolean True)
  Context {variableTypes = fromList [(VariableName (Unicode "foo"),BooleanType)]}

  >>> parse newContext set "foo = 3"
  Set (VariableName (Unicode "foo")) (Number 3.0)
  Context {variableTypes = fromList [(VariableName (Unicode "foo"),NumberType)]}

  >>> parse newContext set "foo = \"hello\""
  Set (VariableName (Unicode "foo")) (String "hello")
  Context {variableTypes = fromList [(VariableName (Unicode "foo"),StringType)]}

  Expressions can be optionally surrounded by braces.

  >>> parse newContext set "foo = { true }"
  Set (VariableName (Unicode "foo")) (Boolean True)
  Context {variableTypes = fromList [(VariableName (Unicode "foo"),BooleanType)]}

  >>> parse newContext set "foo = { 3 }"
  Set (VariableName (Unicode "foo")) (Number 3.0)
  Context {variableTypes = fromList [(VariableName (Unicode "foo"),NumberType)]}

  >>> parse newContext set "foo = { \"hello\" }"
  Set (VariableName (Unicode "foo")) (String "hello")
  Context {variableTypes = fromList [(VariableName (Unicode "foo"),StringType)]}

  You cannot set value of a different type to a variable that already has a
  type.

  >>> parse booleanContext set "boolean = \"hello\""
  1:1:
    |
  1 | boolean = "hello"
    | ^^^^^^^^^^^^^^^^^
  Variable `boolean` cannot be a String; it has already been defined as a Boolean!

  >>> parse numberContext set "number = true"
  1:1:
    |
  1 | number = true
    | ^^^^^^^^^^^^^
  Variable `number` cannot be a Boolean; it has already been defined as a Number!

  >>> parse stringContext set "string = 1.0"
  1:1:
    |
  1 | string = 1.0
    | ^^^^^^^^^^^^
  Variable `string` cannot be a Number; it has already been defined as a String!
-}
set :: Parser Rumor.Node
set = do
  begin <- Mega.getOffset
  name <- hlexeme Identifier.variableName
  _ <- hlexeme (Char.char '=')
  expression <-
    hlexeme
      (     Surround.braces Expression.anyExpression
        <|> Expression.anyExpression
      )
  end <- Mega.getOffset

  typ <- TypeCheck.infer expression
  modifyVariableType name typ begin end
  pure (Rumor.Set name (Rumor.unAnnotate expression))
