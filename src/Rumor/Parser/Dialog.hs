module Rumor.Parser.Dialog
( say
, add
) where

import Data.Text (Text)
import Rumor.Parser.Common (Parser)

import qualified Data.List as List
import qualified Rumor.Internal.Types as Rumor
import qualified Rumor.Parser.Identifier as Identifier
import qualified Rumor.Parser.Lexeme as Lexeme
import qualified Rumor.Parser.Unquoted as Unquoted
import qualified Text.Megaparsec as Mega
import qualified Text.Megaparsec.Char as Char
import qualified Text.Megaparsec.Char.Lexer as Lexer

-- $setup
-- >>> import qualified Text.Megaparsec as Mega
-- >>> let parseTest inner = Mega.parseTest (inner <* Mega.hidden Mega.eof)

{-| Parses an add node, which is defined as an optional identifier for the
  speaker followed by a plus sign and an indented block containing what the
  speaker said. An add node without any speaker is defined as being said by the
  default character (usually the narrator). The indented block can contain
  interpolated values.

  Examples:
  >>> parseTest add "+ Hello world!" -- No speaker
  Add Nothing (String "Hello world!")

  >>> parseTest add "alice+ Hello world!" -- With a speaker
  Add (Just (Speaker "alice")) (String "Hello world!")

  >>> parseTest add "alice+ Hello\n world!" -- Continue in an indented block
  Add (Just (Speaker "alice")) (Concat (String "Hello") (Concat (String " ") (String "world!")))

  >>> parseTest add "alice+\n  Hello world!" -- Start in an indented block
  Add (Just (Speaker "alice")) (String "Hello world!")

  >>> parseTest add "alice+" -- No dialog
  Add (Just (Speaker "alice")) (String "")

  Whitespace handling+
  >>> parseTest add "alice    +    Hello world!" -- Whitespace is okay
  Add (Just (Speaker "alice")) (String "Hello world!")

  >>> parseTest add "alice+ Hello world!  \n" -- Trailing whitespace is ignored
  Add (Just (Speaker "alice")) (String "Hello world!")

  >>> parseTest add "alice+   \n" -- Trailing whitespace is ignored
  Add (Just (Speaker "alice")) (String "")

  Indentation handling:
  >>> parseTest add "alice+\n Hello\n world!\n" -- Same indentation level
  Add (Just (Speaker "alice")) (Concat (String "Hello") (Concat (String " ") (String "world!")))

  >>> parseTest add "alice+\n Hello\n   world!\n" -- Different indentation level
  3:4:
    |
  3 |    world!
    |    ^
  incorrect indentation (got 4, should be equal to 2)

  >>> parseTest add "  alice+ Hello world!  \n" -- You can't start indented
  1:3:
    |
  1 |   alice+ Hello world!  
    |   ^
  incorrect indentation (got 3, should be equal to 1)

  Error examples:
  >>> parseTest add "alice Hello world!  \n"
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
  speaker followed by a plus sign and an indented block containing what the
  speaker said. A say node without any speaker is defined as being said by the
  default character (usually the narrator). The indented block can contain
  interpolated values.

  Examples:
  >>> parseTest say ": Hello world!" -- No speaker
  Say Nothing (String "Hello world!")

  >>> parseTest say "alice: Hello world!" -- With a speaker
  Say (Just (Speaker "alice")) (String "Hello world!")

  >>> parseTest say "alice: Hello\n world!" -- Continue in an indented block
  Say (Just (Speaker "alice")) (Concat (String "Hello") (Concat (String " ") (String "world!")))

  >>> parseTest say "alice:\n  Hello world!" -- Start in an indented block
  Say (Just (Speaker "alice")) (String "Hello world!")

  >>> parseTest say "alice:" -- No dialog
  Say (Just (Speaker "alice")) (String "")

  Whitespace handling:
  >>> parseTest say "alice    :    Hello world!" -- Whitespace is okay
  Say (Just (Speaker "alice")) (String "Hello world!")

  >>> parseTest say "alice: Hello world!  \n" -- Trailing whitespace is ignored
  Say (Just (Speaker "alice")) (String "Hello world!")

  >>> parseTest say "alice:   \n" -- Trailing whitespace is ignored
  Say (Just (Speaker "alice")) (String "")

  Indentation handling:
  >>> parseTest say "alice:\n Hello\n world!\n" -- Same indentation level
  Say (Just (Speaker "alice")) (Concat (String "Hello") (Concat (String " ") (String "world!")))

  >>> parseTest say "alice:\n Hello\n   world!\n" -- Different indentation level
  3:4:
    |
  3 |    world!
    |    ^
  incorrect indentation (got 4, should be equal to 2)

  >>> parseTest say "  alice: Hello world!  \n" -- You can't start indented
  1:3:
    |
  1 |   alice: Hello world!  
    |   ^
  incorrect indentation (got 3, should be equal to 1)

  Error examples:
  >>> parseTest say "alice Hello world!  \n"
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
  (Maybe Rumor.Speaker -> Rumor.Expression Text -> Rumor.Node) ->
  Parser Rumor.Node
dialog sep cons =
  let
    mkDialog speaker texts =
      cons
        (Rumor.Speaker <$> speaker)
        ( mconcat
            ( List.intersperse
                (Rumor.String " ")
                (List.filter (/= mempty) texts)
            )
        )

    dialogIndent = do
      speaker <- Lexeme.hlexeme (Mega.optional Identifier.identifier)
      _ <- Lexeme.hlexeme (Char.char sep)
      firstText <- Unquoted.unquotedLine
      pure
        ( Lexer.IndentMany
            Nothing
            (\texts -> pure (mkDialog speaker (firstText:texts)))
            Unquoted.unquotedLine
        )

  in do
    -- Make sure we aren't indented
    _ <- Lexer.indentGuard Lexeme.space EQ =<< Lexer.indentLevel

    Lexer.indentBlock Lexeme.space dialogIndent
