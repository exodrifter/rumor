module Rumor.Parser.Choice
( choice
) where

import Rumor.Parser.Common (Parser, eolf, hlexeme, space)

import qualified Text.Megaparsec.Char as Char
import qualified Text.Megaparsec.Char.Lexer as Lexer
import qualified Rumor.Internal as Rumor
import qualified Rumor.Parser.Indented as Indented
import qualified Rumor.Parser.Unquoted as Unquoted

-- $setup
-- >>> import Rumor.Parser.Common
-- >>> import Rumor.Parser.Dialog (say)
-- >>> let parse inner = parseTest newContext (inner <* eof)

{-| Parses a choice which has an unquoted string used as the display text for
  the choice. It can optionally contain an indented block of nodes.

  The choice text can be provided on one or multiple lines, like unquoted
  strings for dialog nodes.

  >>> parse (choice say) "choice\n  > foo bar baz\n  : Hello"
  Choice (Concat (String "foo") (String " bar baz")) Nothing (Say Nothing (String "Hello") Nothing :| [])

  >>> parse (choice say) "choice\n  > foo\n    bar\n    baz\n  : Hello"
  Choice (Concat (String "foo") (Concat (String " ") (Concat (String "bar") (Concat (String " ") (String "baz"))))) Nothing (Say Nothing (String "Hello") Nothing :| [])

  The choice text can be provided on the following line, but it must have
  indentation greater than the choice marker '>'.

  >>> parse (choice say) "choice\n  >\n    foo\n    bar\n    baz\n  : Hello"
  Choice (Concat (String "foo") (Concat (String " ") (Concat (String "bar") (Concat (String " ") (String "baz"))))) Nothing (Say Nothing (String "Hello") Nothing :| [])

  >>> parse (choice say) "choice\n  >\nfoo"
  3:1:
    |
  3 | foo
    | ^
  incorrect indentation (got 1, should be greater than 3)

  The choice text can be given a label.

  >>> parse (choice say) "choice\n  > foo bar baz [label]\n  : Hello"
  Choice (Concat (String "foo") (String " bar baz")) (Just (Label (Unicode "label"))) (Say Nothing (String "Hello") Nothing :| [])

  >>> parse (choice say) "choice\n  > foo\n    bar\n    baz [label]\n  : Hello"
  Choice (Concat (String "foo") (Concat (String " ") (Concat (String "bar") (Concat (String " ") (String "baz"))))) (Just (Label (Unicode "label"))) (Say Nothing (String "Hello") Nothing :| [])

  >>> parse (choice say) "choice\n  >\n    foo\n    bar\n    baz [label]\n  : Hello"
  Choice (Concat (String "foo") (Concat (String " ") (Concat (String "bar") (Concat (String " ") (String "baz"))))) (Just (Label (Unicode "label"))) (Say Nothing (String "Hello") Nothing :| [])

  >>> parse (choice say) "choice\n  >\n    foo\n    bar\n    baz\n    [label]\n  : Hello"
  Choice (Concat (String "foo") (Concat (String " ") (Concat (String "bar") (Concat (String " ") (String "baz"))))) (Just (Label (Unicode "label"))) (Say Nothing (String "Hello") Nothing :| [])

  Multi-line choice text must have the same level of indentation.

  >>> parse (choice say) "choice\n  > foo\n    bar\n      baz"
  4:7:
    |
  4 |       baz
    |       ^
  incorrect indentation (got 7, should be equal to 5)

  >>> parse (choice say) "choice\n  > foo\n    bar\n baz"
  4:2:
    |
  4 |  baz
    |  ^
  incorrect indentation (got 2, should be equal to 3)

  All of the nodes in the indented block have to have the same amount of
  indentation as the choice text.

  >>> parse (choice say) "choice\n  > Choice\n  : foo\n  : bar\n  :baz"
  Choice (String "Choice") Nothing (Say Nothing (String "foo") Nothing :| [Say Nothing (String "bar") Nothing,Say Nothing (String "baz") Nothing])

  >>> parse (choice say) "choice\n  > Choice\n: Hello"
  3:1:
    |
  3 | : Hello
    | ^
  incorrect indentation (got 1, should be equal to 3)
-}
choice :: Parser Rumor.Node -> Parser Rumor.Node
choice inner = do
  originalRef <- Lexer.indentLevel
  _ <- Lexer.indentGuard space EQ originalRef

  _ <- hlexeme "choice"
  eolf

  indentedRef <- Lexer.indentGuard space GT originalRef
  _ <- hlexeme (Char.char '>')
  (choiceText, label) <- Unquoted.unquoted indentedRef

  indentedNodes <- Indented.someIndentedAt indentedRef inner

  pure (Rumor.Choice choiceText label indentedNodes)
