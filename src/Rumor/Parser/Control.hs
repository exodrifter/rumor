module Rumor.Parser.Control
( control
) where

import Data.Text (Text)
import Rumor.Parser.Common (Parser, eolf, hlexeme, space, (<|>))
import Data.List.NonEmpty (NonEmpty(..))

import qualified Text.Megaparsec as Mega
import qualified Text.Megaparsec.Char as Char
import qualified Text.Megaparsec.Char.Lexer as Lexer
import qualified Rumor.Internal.Types as Rumor
import qualified Rumor.Parser.Surround as Surround
import qualified Rumor.Parser.Expression as Expression
import qualified Rumor.Parser.Indented as Indented

-- $setup
-- >>> import qualified Text.Megaparsec as Mega
-- >>> import Rumor.Parser.Common (hspace)
-- >>> import Rumor.Parser.Dialog (say)
-- >>> let parseTest inner = Mega.parseTest (inner <* Mega.hidden Mega.eof)

{-| Parses a conditional control statement at the current indentation level.

  You can have if statements that have an indented block of nodes.

  >>> parseTest (control say) "if true\n  : foo"
  Control (Boolean True) (Say Nothing (String "foo") :| []) Nothing

  >>> parseTest (control say) "if true or false\n  : foo\n  : foo\n  : foo"
  Control (LogicalOr (Boolean True) (Boolean False)) (Say Nothing (String "foo") :| [Say Nothing (String "foo"),Say Nothing (String "foo")]) Nothing

  >>> parseTest (control say) "if true\n\n\n  : foo"
  Control (Boolean True) (Say Nothing (String "foo") :| []) Nothing

  >>> parseTest (control say) "if true\n: foo"
  2:1:
    |
  2 | : foo
    | ^
  incorrect indentation (got 1, should be greater than 1)

  The condition in the if statement can be optionally surrounded by braces.

  >>> parseTest (control say) "if { true }\n  : foo"
  Control (Boolean True) (Say Nothing (String "foo") :| []) Nothing

  >>> parseTest (control say) "if {true or false}\n  : foo"
  Control (LogicalOr (Boolean True) (Boolean False)) (Say Nothing (String "foo") :| []) Nothing

  >>> parseTest (control say) "if {true or false\n  : foo"
  2:3:
    |
  2 |   : foo
    |   ^
  unexpected ':'
  expecting close brace

  >>> parseTest (control say) "if true or false}\n  : foo"
  1:17:
    |
  1 | if true or false}
    |                 ^^
  unexpected "}<newline>"
  expecting "!=", "&&", "/=", "==", "and", "or", "xor", "||", '^', carriage return, crlf newline, end of input, or newline

  You cannot have an empty block of nodes.

  >>> parseTest (control say) "if true"
  1:8:
    |
  1 | if true
    |        ^
  unexpected end of input
  expecting "!=", "&&", "/=", "==", "and", "or", "xor", "||", ':', '^', carriage return, crlf newline, identifier, or newline

  >>> parseTest (control say) "if true\n"
  2:1:
    |
  2 | <empty line>
    | ^
  incorrect indentation (got 1, should be greater than 1)

  >>> parseTest (control say) "if true\n  "
  2:3:
    |
  2 |   
    |   ^
  unexpected end of input
  expecting ':' or identifier

  You can chain if statements into elif statements, else statements, or both.

  >>> parseTest (control say) "if false\n  : foo\nelif true\n  : bar"
  Control (Boolean False) (Say Nothing (String "foo") :| []) (Just (Control (Boolean True) (Say Nothing (String "bar") :| []) Nothing :| []))

  >>> parseTest (control say) "if false\n  : foo\nelse\n  :bar"
  Control (Boolean False) (Say Nothing (String "foo") :| []) (Just (Say Nothing (String "bar") :| []))

  >>> parseTest (control say) "if false\n  : foo\nelif true\n  : bar\nelse\n  : baz"
  Control (Boolean False) (Say Nothing (String "foo") :| []) (Just (Control (Boolean True) (Say Nothing (String "bar") :| []) (Just (Say Nothing (String "baz") :| [])) :| []))

  All of the control statements must have the same indentation level.

  >>> parseTest (control say) "if false\n  : foo\n  elif true\n  : bar"
  3:3:
    |
  3 |   elif true
    |   ^
  unexpected 'e'

  >>> parseTest (control say) "if false\n  : foo\n  else\n  :bar"
  3:3:
    |
  3 |   else
    |   ^
  unexpected 'e'

  >>> parseTest (control say) "if false\n  : foo\nelif true\n  : bar\n  else\n  : baz"
  5:3:
    |
  5 |   else
    |   ^
  unexpected 'e'

  This parser consumes all trailing whitespace.

  >>> parseTest (control say) "if true\n  : foo  "
  Control (Boolean True) (Say Nothing (String "foo") :| []) Nothing

  >>> parseTest (control say) "if true\n  : foo  \n"
  Control (Boolean True) (Say Nothing (String "foo") :| []) Nothing

  >>> parseTest (control say) "if true\n  : foo  \n  "
  Control (Boolean True) (Say Nothing (String "foo") :| []) Nothing

  >>> parseTest (control say) "if false\n  : foo\nelif true\n  : bar  "
  Control (Boolean False) (Say Nothing (String "foo") :| []) (Just (Control (Boolean True) (Say Nothing (String "bar") :| []) Nothing :| []))

  >>> parseTest (control say) "if false\n  : foo\nelif true\n  : bar  \n"
  Control (Boolean False) (Say Nothing (String "foo") :| []) (Just (Control (Boolean True) (Say Nothing (String "bar") :| []) Nothing :| []))

  >>> parseTest (control say) "if false\n  : foo\nelif true\n  : bar  \n  "
  Control (Boolean False) (Say Nothing (String "foo") :| []) (Just (Control (Boolean True) (Say Nothing (String "bar") :| []) Nothing :| []))

  >>> parseTest (control say) "if false\n  : foo\nelse\n  :bar  "
  Control (Boolean False) (Say Nothing (String "foo") :| []) (Just (Say Nothing (String "bar") :| []))

  >>> parseTest (control say) "if false\n  : foo\nelse\n  :bar  \n"
  Control (Boolean False) (Say Nothing (String "foo") :| []) (Just (Say Nothing (String "bar") :| []))

  >>> parseTest (control say) "if false\n  : foo\nelse\n  :bar  \n  "
  Control (Boolean False) (Say Nothing (String "foo") :| []) (Just (Say Nothing (String "bar") :| []))

  >>> parseTest (control say) "if false\n  : foo\nelif true\n  : bar\nelse\n  : baz  "
  Control (Boolean False) (Say Nothing (String "foo") :| []) (Just (Control (Boolean True) (Say Nothing (String "bar") :| []) (Just (Say Nothing (String "baz") :| [])) :| []))

  >>> parseTest (control say) "if false\n  : foo\nelif true\n  : bar\nelse\n  : baz  \n"
  Control (Boolean False) (Say Nothing (String "foo") :| []) (Just (Control (Boolean True) (Say Nothing (String "bar") :| []) (Just (Say Nothing (String "baz") :| [])) :| []))

  >>> parseTest (control say) "if false\n  : foo\nelif true\n  : bar\nelse\n  : baz  \n  "
  Control (Boolean False) (Say Nothing (String "foo") :| []) (Just (Control (Boolean True) (Say Nothing (String "bar") :| []) (Just (Say Nothing (String "baz") :| [])) :| []))
-}
control :: Parser Rumor.Node -> Parser Rumor.Node
control inner = do
  -- All of the control statements must have the same indentation
  ref <- Lexer.indentLevel
  controlIf "if" ref inner

{-| Parses a conditional control statement, which must be at the given
  indentation level.

  >>> parseTest (controlIf "if" (Mega.mkPos 1) say) "if false\n  : foo"
  Control (Boolean False) (Say Nothing (String "foo") :| []) Nothing

  >>> parseTest (controlIf "if" (Mega.mkPos 3) say) "if false\n  : foo"
  1:1:
    |
  1 | if false
    | ^
  incorrect indentation (got 1, should be equal to 3)

  >>> parseTest (do hspace; controlIf "if" (Mega.mkPos 1) say) "  if false\n  : foo"
  1:3:
    |
  1 |   if false
    |   ^
  incorrect indentation (got 3, should be equal to 1)
-}
controlIf :: Text -> Mega.Pos -> Parser Rumor.Node -> Parser Rumor.Node
controlIf name ref inner = do
  _ <- Lexer.indentGuard space EQ ref
  _ <- hlexeme (Char.string name)
  condition <-
    hlexeme
      (     Surround.braces Expression.booleanExpression
        <|> Expression.booleanExpression
      )
  _ <- eolf

  successBlock <- Indented.someIndentedMoreThan ref inner
  failureBlock <- Mega.optional
      (     Mega.try ((:| []) <$> controlIf "elif" ref inner)
        <|> Mega.try (controlElse ref inner)
      )

  pure (Rumor.Control condition successBlock failureBlock)

{-| Parses an else statement, which must be at the given indentation level, and
  a non-empty list of inner elements.

  >>> parseTest (controlElse (Mega.mkPos 1) "foo") "else\n  foo"
  "foo" :| []

  >>> parseTest (controlElse (Mega.mkPos 1) "foo") "else\n  foo\n  foo\n  foo"
  "foo" :| ["foo","foo"]

  >>> parseTest (controlElse (Mega.mkPos 1) "foo") "else\n\n\n  foo"
  "foo" :| []

  The else statement must match the specified indentation level

  >>> parseTest (controlElse (Mega.mkPos 3) "foo") "else\nfoo"
  1:1:
    |
  1 | else
    | ^
  incorrect indentation (got 1, should be equal to 3)

  >>> parseTest (do hspace; controlElse (Mega.mkPos 1) "foo") "  else\nfoo"
  1:3:
    |
  1 |   else
    |   ^
  incorrect indentation (got 3, should be equal to 1)

  The inner elements have to be on an indented line following the else
  statement.

  >>> parseTest (controlElse (Mega.mkPos 1) "foo") "else\n\n\n  foo"
  "foo" :| []

  >>> parseTest (controlElse (Mega.mkPos 1) "foo") "else\nfoo"
  2:1:
    |
  2 | foo
    | ^
  incorrect indentation (got 1, should be greater than 1)

  >>> parseTest (controlElse (Mega.mkPos 3) "foo") "  else\nfoo"
  2:1:
    |
  2 | foo
    | ^
  incorrect indentation (got 1, should be greater than 3)

  >>> parseTest (controlElse (Mega.mkPos 1) "foo") "else      foo"
  1:11:
    |
  1 | else      foo
    |           ^
  unexpected 'f'
  expecting newline
-}
controlElse :: Mega.Pos -> Parser a -> Parser (NonEmpty a)
controlElse ref inner = do
  _ <- Lexer.indentGuard space EQ ref
  _ <- hlexeme (Char.string "else")
  _ <- Char.char '\n'

  Indented.someIndentedMoreThan ref inner
