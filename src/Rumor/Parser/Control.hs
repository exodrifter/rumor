module Rumor.Parser.Control
( control
) where

import Data.Text (Text)
import Rumor.Parser.Common (Parser, (<|>))
import Data.List.NonEmpty (NonEmpty(..))

import qualified Text.Megaparsec as Mega
import qualified Text.Megaparsec.Char as Char
import qualified Text.Megaparsec.Char.Lexer as Lexer
import qualified Rumor.Internal.Types as Rumor
import qualified Rumor.Parser.Lexeme as Lexeme
import qualified Rumor.Parser.Surround as Surround
import qualified Rumor.Parser.Expression as Expression
import qualified Rumor.Parser.Indented as Indented

-- $setup
-- >>> import qualified Text.Megaparsec as Mega
-- >>> let parseTest inner = Mega.parseTest (inner <* Mega.hidden Mega.eof)

{-| Parses a conditional control statement at the current indentation level

  Examples:
  >>> import Rumor.Parser.Dialog (say)
  >>> parseTest (control say) "if true\n  : foo"
  Control (Boolean True) (Say Nothing (String "foo") :| []) Nothing

  >>> parseTest (control say) "  if true\n: foo"
  1:3:
    |
  1 |   if true
    |   ^
  incorrect indentation (got 3, should be equal to 1)
-}
control :: Parser Rumor.Node -> Parser Rumor.Node
control inner = do
  -- All of the control statements must have the same indentation
  ref <- Lexer.indentLevel
  _ <- Lexer.indentGuard Lexeme.space EQ ref

  controlIf "if" ref inner

{-| Parses a conditional control statement, which must be at the given
  indentation level.

  If examples:
  >>> import Rumor.Parser.Dialog (say)
  >>> let parse pos = parseTest (controlIf "if" (Mega.mkPos pos) say)
  >>> parse 1 "if true\n  : foo"
  Control (Boolean True) (Say Nothing (String "foo") :| []) Nothing

  >>> parse 1 "if true or false\n  : foo\n  : foo\n  : foo"
  Control (LogicalOr (Boolean True) (Boolean False)) (Say Nothing (String "foo") :| [Say Nothing (String "foo"),Say Nothing (String "foo")]) Nothing

  >>> parse 1 "if true\n\n\n  : foo" -- multiple newlines are okay
  Control (Boolean True) (Say Nothing (String "foo") :| []) Nothing

  >>> parse 3 "if true\n: foo"
  1:1:
    |
  1 | if true
    | ^
  incorrect indentation (got 1, should be equal to 3)

  >>> parse 1 "if true\n: foo"
  2:1:
    |
  2 | : foo
    | ^
  incorrect indentation (got 1, should be greater than 1)

  >>> parse 3 "  if true\n: foo"
  2:1:
    |
  2 | : foo
    | ^
  incorrect indentation (got 1, should be greater than 3)

  >>> parse 1 "if true      : foo"
  1:14:
    |
  1 | if true      : foo
    |              ^
  unexpected ':'
  expecting newline

  Else examples:
  >>> parse 1 "if true\n  : foo\nelse\n  : bar"
  Control (Boolean True) (Say Nothing (String "foo") :| []) (Just (Say Nothing (String "bar") :| []))

  >>> parse 1 "if true or false\n  : foo\n  : foo\nelse\n  : bar\n  : bar"
  Control (LogicalOr (Boolean True) (Boolean False)) (Say Nothing (String "foo") :| [Say Nothing (String "foo")]) (Just (Say Nothing (String "bar") :| [Say Nothing (String "bar")]))

  >>> parse 1 "if true\n\n\n  : foo\n\n\nelse\n\n\n  : bar" -- multiple newlines are okay
  Control (Boolean True) (Say Nothing (String "foo") :| []) (Just (Say Nothing (String "bar") :| []))

  >>> parse 1 "if true\n  : foo\n else\n  : bar"
  3:2:
    |
  3 |  else
    |  ^
  incorrect indentation (got 2, should be equal to 1)

  >>> parse 3 "  if true\n    : foo\nelse\n  : bar"
  3:1:
    |
  3 | else
    | ^
  unexpected 'e'

  Elif examples:
  >>> parse 1 "if true\n  : foo\nelif false\n  : bar"
  Control (Boolean True) (Say Nothing (String "foo") :| []) (Just (Control (Boolean False) (Say Nothing (String "bar") :| []) Nothing :| []))

  >>> parse 1 "if true\n  : foo\nelif false\n  : bar\nelif true\n  : baz\nelse\n  : biz"
  Control (Boolean True) (Say Nothing (String "foo") :| []) (Just (Control (Boolean False) (Say Nothing (String "bar") :| []) (Just (Control (Boolean True) (Say Nothing (String "baz") :| []) (Just (Say Nothing (String "biz") :| [])) :| [])) :| []))

  >>> parse 1 "if true\n\n\n  : foo\n\n\nelif false\n\n\n  : bar" -- multiple newlines are okay
  Control (Boolean True) (Say Nothing (String "foo") :| []) (Just (Control (Boolean False) (Say Nothing (String "bar") :| []) Nothing :| []))

  >>> parse 1 "if true\n  : foo\n elif false\n  : bar"
  3:2:
    |
  3 |  elif false
    |  ^
  incorrect indentation (got 2, should be equal to 1)

  >>> parse 3 "  if true\n    : foo\nelif false\n  : bar"
  3:1:
    |
  3 | elif false
    | ^
  unexpected 'e'
-}
controlIf :: Text -> Mega.Pos -> Parser Rumor.Node -> Parser Rumor.Node
controlIf name ref inner = do
  _ <- Lexer.indentGuard Lexeme.space EQ ref
  _ <- Lexeme.hlexeme (Char.string name)
  condition <-
    Lexeme.hlexeme
      (     Surround.braces Expression.booleanExpression
        <|> Expression.booleanExpression
      )
  _ <- Char.char '\n'

  successBlock <- Indented.someIndentedMoreThan ref inner
  failureBlock <-
    Mega.optional
      (     Mega.try ((:| []) <$> controlIf "elif" ref inner)
        <|> controlElse ref inner
      )

  pure (Rumor.Control condition successBlock failureBlock)

{-| Parses an else statement, which must be at the given indentation level.

  Examples:
  >>> let parse pos = parseTest (controlElse (Mega.mkPos pos) "foo")
  >>> parse 1 "else\n  foo"
  "foo" :| []

  >>> parse 1 "else\n  foo\n  foo\n  foo"
  "foo" :| ["foo","foo"]

  >>> parse 1 "else\n\n\n  foo" -- multiple newlines are okay
  "foo" :| []

  Error Examples:
  >>> parse 3 "else\nfoo"
  1:1:
    |
  1 | else
    | ^
  incorrect indentation (got 1, should be equal to 3)

  >>> parse 1 "else\nfoo"
  2:1:
    |
  2 | foo
    | ^
  incorrect indentation (got 1, should be greater than 1)

  >>> parse 3 "  else\nfoo"
  2:1:
    |
  2 | foo
    | ^
  incorrect indentation (got 1, should be greater than 3)

  >>> parse 1 "else      foo"
  1:11:
    |
  1 | else      foo
    |           ^
  unexpected 'f'
  expecting newline
-}
controlElse :: Mega.Pos -> Parser a -> Parser (NonEmpty a)
controlElse ref inner = do
  _ <- Lexer.indentGuard Lexeme.space EQ ref
  _ <- Lexeme.hlexeme (Char.string "else")
  _ <- Char.char '\n'

  Indented.someIndentedMoreThan ref inner
