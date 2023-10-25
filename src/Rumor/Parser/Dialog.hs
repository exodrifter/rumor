module Rumor.Parser.Dialog
( say
, add
) where

import Data.Text (Text)
import Rumor.Parser.Common (Parser, hlexeme)

import qualified Rumor.Internal as Rumor
import qualified Rumor.Parser.Identifier as Identifier
import qualified Rumor.Parser.Unquoted as Unquoted
import qualified Text.Megaparsec as Mega
import qualified Text.Megaparsec.Char as Char
import qualified Text.Megaparsec.Char.Lexer as Lexer

-- $setup
-- >>> import Rumor.Parser.Common
-- >>> let parse inner = parseTest newContext (inner <* eof)

{-| Parses an add node, which is defined as an optional identifier for the
  speaker followed by a plus sign and an indented unquoted string containing
  what the speaker said.

  Add nodes can have one line.

  >>> parse add "alice+ Hello world!    \n"
  Add (Just (Speaker (Unicode "alice"))) (String "Hello world!") Nothing

  >>> parse add "+ Hello world!"
  Add Nothing (String "Hello world!") Nothing

  >>> parse add "alice+ Hello world!" -- With a speaker
  Add (Just (Speaker (Unicode "alice"))) (String "Hello world!") Nothing

  Add nodes can be spread over multiple lines, or start indented on the next
  line.

  >>> parse add "alice+ foo\n  bar\n  baz"
  Add (Just (Speaker (Unicode "alice"))) (String "foo bar baz") Nothing

  >>> parse add "alice+\n  foo\n  bar\n  baz"
  Add (Just (Speaker (Unicode "alice"))) (String "foo bar baz") Nothing

  Extra whitespace is okay and trailing whitespace is consumed.

  >>> parse add "alice    +    Hello world!"
  Add (Just (Speaker (Unicode "alice"))) (String "Hello world!") Nothing

  >>> parse add "alice+ Hello world!    "
  Add (Just (Speaker (Unicode "alice"))) (String "Hello world!") Nothing


  >>> parse add "alice+ Hello world!    \n    "
  Add (Just (Speaker (Unicode "alice"))) (String "Hello world!") Nothing

  >>> parse add "alice+\n  Hello world!\n  "
  Add (Just (Speaker (Unicode "alice"))) (String "Hello world!") Nothing

  Add nodes end when the following line is unindented.

  >>> parse add "alice+ Hello\nworld!\n"
  2:1:
    |
  2 | world!
    | ^
  unexpected 'w'
  expecting end of input

  Add nodes cannot be empty.

  >>> parse add "+"
  1:2:
    |
  1 | +
    |  ^
  unexpected end of input
  expecting end of line or unquoted line

  >>> parse add "+\n  "
  2:3:
    |
  2 |   
    |   ^
  unexpected end of input
  expecting unquoted line

  Add nodes must have a separator.

  >>> parse add "alice Hello world!  \n"
  1:7:
    |
  1 | alice Hello world!  
    |       ^
  unexpected 'H'
  expecting '+'
-}
add :: Parser Rumor.Node
add = dialog '+' Rumor.Add

{-| Parses a say node, which is defined as an optional identifier for the
  speaker followed by a colon and an indented unquoted string containing what
  the speaker said.

  Say nodes can have one line.

  >>> parse say "alice: Hello world!    \n"
  Say (Just (Speaker (Unicode "alice"))) (String "Hello world!") Nothing

  >>> parse say ": Hello world!"
  Say Nothing (String "Hello world!") Nothing

  >>> parse say "alice: Hello world!" -- With a speaker
  Say (Just (Speaker (Unicode "alice"))) (String "Hello world!") Nothing

  Say nodes can be spread over multiple lines, or start indented on the next
  line.

  >>> parse say "alice: foo\n  bar\n  baz"
  Say (Just (Speaker (Unicode "alice"))) (String "foo bar baz") Nothing

  >>> parse say "alice:\n  foo\n  bar\n  baz"
  Say (Just (Speaker (Unicode "alice"))) (String "foo bar baz") Nothing

  Extra whitespace is okay and trailing whitespace is consumed.

  >>> parse say "alice    :    Hello world!"
  Say (Just (Speaker (Unicode "alice"))) (String "Hello world!") Nothing

  >>> parse say "alice: Hello world!    "
  Say (Just (Speaker (Unicode "alice"))) (String "Hello world!") Nothing


  >>> parse say "alice: Hello world!    \n    "
  Say (Just (Speaker (Unicode "alice"))) (String "Hello world!") Nothing

  >>> parse say "alice:\n  Hello world!\n  "
  Say (Just (Speaker (Unicode "alice"))) (String "Hello world!") Nothing

  Say nodes end when the following line is unindented.

  >>> parse say "alice: Hello\nworld!\n"
  2:1:
    |
  2 | world!
    | ^
  unexpected 'w'
  expecting end of input

  Say nodes cannot be empty.

  >>> parse say ":"
  1:2:
    |
  1 | :
    |  ^
  unexpected end of input
  expecting end of line or unquoted line

  >>> parse say ":\n  "
  2:3:
    |
  2 |   
    |   ^
  unexpected end of input
  expecting unquoted line

  Say nodes must have a separator.

  >>> parse say "alice Hello world!  \n"
  1:7:
    |
  1 | alice Hello world!  
    |       ^
  unexpected 'H'
  expecting ':'
-}
say :: Parser Rumor.Node
say = dialog ':' Rumor.Say

dialog ::
  Char ->
  (Maybe Rumor.Speaker -> Rumor.Expression Text -> Maybe Rumor.Label -> Rumor.Node) ->
  Parser Rumor.Node
dialog sep constructor = do
  ref <- Lexer.indentLevel

  speaker <- Mega.optional (Rumor.Speaker <$> hlexeme Identifier.identifier)
  _ <- hlexeme (Char.char sep)
  (text, label) <- Unquoted.unquoted ref

  pure (constructor speaker text label)
