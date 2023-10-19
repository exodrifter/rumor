module Rumor.Parser.Unquoted
( unquotedBlock
, unquotedLine
) where

import Data.Text (Text)
import Rumor.Parser.Common (Parser, hlexeme, hspace, space, (<|>))

import qualified Data.List as List
import qualified Data.Text as T
import qualified Rumor.Internal.Types as Rumor
import qualified Rumor.Parser.Expression as Expression
import qualified Rumor.Parser.Identifier as Identifier
import qualified Text.Megaparsec as Mega
import qualified Text.Megaparsec.Char as Char
import qualified Text.Megaparsec.Char.Lexer as Lexer

-- $setup
-- >>> import qualified Text.Megaparsec as Mega
-- >>> let parseTest inner = Mega.parseTest (inner <* Mega.hidden Mega.eof)

{-| Parses an unquoted block, which contains an identifying starting parser
  followed by multiple indented unquoted string literals on the same or
  following lines ending with an optional label. An unquoted line will have
  all trailing horizontal whitespace on each line removed.

  >>> let block pos = unquotedBlock (Mega.mkPos pos)
  >>> parseTest (block 1) "Hello world!"
  String "Hello world!"

  >>> parseTest (block 1) "I have { 5 } mangoes!"
  Concat (String "I have ") (Concat (NumberToString (Number 5.0)) (String " mangoes!"))

  >>> parseTest (block 1) "Hello\\nworld!"
  Concat (String "Hello") (Concat (String "\n") (String "world!"))

  >>> parseTest (block 1) "Hello\n world!" -- Indented block
  Concat (String "Hello") (Concat (String " ") (String "world!"))

  >>> parseTest (block 1) "foo\n  bar\n  baz" -- Indented block
  Concat (String "foo") (Concat (String " ") (Concat (String "bar") (Concat (String " ") (String "baz"))))

  Whitespace handling:
  >>> parseTest (block 1) "Hello world!   " -- Trailing whitespace is removed
  String "Hello world!"

  >>> parseTest (block 1) "Hello  \n world!  " -- Trailing whitespace is removed
  Concat (String "Hello") (Concat (String " ") (String "world!"))

  >>> parseTest (block 1) "Hello  \n world!  \n" -- The trailing newlines is consumed
  Concat (String "Hello") (Concat (String " ") (String "world!"))

  Error examples:
  >>> parseTest (block 1) "foo\n  bar\n    baz" -- Indentation must match
  3:5:
    |
  3 |     baz
    |     ^
  incorrect indentation (got 5, should be equal to 3)
-}
unquotedBlock :: Mega.Pos -> Parser (Rumor.Expression Text)
unquotedBlock ref = do
  -- Check if there is content on this line
  hspace
  firstLine <- Mega.try (Just <$> unquotedLine) <|> (do eol; pure Nothing)

  let
    combine texts =
      mconcat
        ( List.intersperse
            (Rumor.String " ")
            (List.filter (/= mempty) texts)
        )
    unindented = do
      _ <- Mega.try (Lexer.indentGuard space LT ref)
        <|> Lexer.indentGuard space EQ ref
      pure ()
    indentedUnquotedLine r = do
      _ <- Lexer.indentGuard space EQ r
      fst <$> unquotedLine

  case firstLine of

    -- In this case, there must be at least one indented line.
    Nothing -> do
      newIndentedRef <- Lexer.indentGuard space GT ref
      rest <-
        Mega.someTill
          (indentedUnquotedLine newIndentedRef)
          (Mega.try unindented <|> eol)
      pure (combine rest)

    -- In this case, the indented lines are optional.
    Just (first, _) -> do
      newIndentedRef <-
        Mega.lookAhead (do space; Lexer.indentLevel)

      -- Check for more lines.
      rest <- do
        if newIndentedRef > ref
        then
          Mega.manyTill
            (indentedUnquotedLine newIndentedRef)
            (Mega.try unindented <|> eol)
        else
          pure []

      pure (combine (first : rest))

{-| Parses an unquoted line, which is an unquoted string literal which may end
  with a label. An unquoted line will have all trailing horizontal whitespace
  removed.

  >>> parseTest unquotedLine "I have { 5 } mangoes!"
  (Concat (String "I have ") (Concat (NumberToString (Number 5.0)) (String " mangoes!")),Nothing)

  >>> parseTest unquotedLine "Hello world!"
  (String "Hello world!",Nothing)

  >>> parseTest unquotedLine "Hello world! [label]"
  (String "Hello world!",Just (Label "label"))

  >>> parseTest unquotedLine "Hello world! \\[label\\]" -- You can escape brackets
  (Concat (String "Hello world! ") (Concat (String "[") (Concat (String "label") (String "]"))),Nothing)

  >>> parseTest unquotedLine "Hello\\nworld!"
  (Concat (String "Hello") (Concat (String "\n") (String "world!")),Nothing)

  Whitespace handling:
  >>> parseTest unquotedLine "Hello world!   " -- Trailing whitespace is removed
  (String "Hello world!",Nothing)

  >>> parseTest unquotedLine "Hello world!   [label]   " -- Trailing whitespace is removed
  (String "Hello world!",Just (Label "label"))

  >>> parseTest unquotedLine "Hello world!\n" -- The trailing newline is consumed
  (String "Hello world!",Nothing)

  >>> parseTest unquotedLine "Hello world!   [label]   \n" -- The trailing newline is consumed
  (String "Hello world!",Just (Label "label"))

  >>> parseTest unquotedLine "Hello\nworld!" -- Newlines are not okay
  2:1:
    |
  2 | world!
    | ^
  unexpected 'w'

  Error example:
  >>> parseTest unquotedLine "" -- No dialog is not okay
  1:1:
    |
  1 | <empty line>
    | ^
  unexpected end of input
  expecting '\', interpolation, or literal char

  >>> parseTest unquotedLine "[label]" -- Just a label is not okay
  1:1:
    |
  1 | [label]
    | ^
  unexpected '['
  expecting '\', interpolation, or literal char

  >>> parseTest unquotedLine "Hello world! ["
  1:15:
    |
  1 | Hello world! [
    |               ^
  unexpected end of input
  expecting identifier
-}
unquotedLine :: Parser (Rumor.Expression Text, Maybe Rumor.Label)
unquotedLine = do
  let
    end = (do _ <- Char.char '['; pure ()) <|> eol

    literalString = do
      literal <-
        Mega.takeWhile1P
          (Just "literal char")
          (`notElem` (fst <$> unquotedEscapes))

      -- Strip the end of the text if this is at the end of the line
      next <- Mega.lookAhead ((do end; pure True) <|> pure False)
      if next
      then do
        pure (Rumor.String (T.stripEnd literal))
      else pure (Rumor.String literal)

  text <- Mega.some
    (     literalString
      <|> Expression.escape unquotedEscapes
      <|> Expression.interpolation
    )

  label <- Mega.optional (hlexeme Identifier.label)
  _ <- Mega.try eol
  pure (mconcat text, label)

unquotedEscapes :: [(Char, Text)]
unquotedEscapes =
  [ ('[', "[")
  , (']', "]")
  ] <> Expression.stringEscapes

{-| Parses a newline or the end of the file.
-}
eol :: Parser ()
eol =
      (do _ <- Char.char '\n'; pure ())
  <|> (do _ <- Char.char '\r'; pure ())
  <|> Mega.eof
