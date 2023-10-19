module Rumor.Parser.Unquoted
( unquoted
) where

import Data.Char (isSpace)
import Data.Text (Text)
import Rumor.Parser.Common (Parser, eolf, space, (<?>), (<|>))

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
-- >>> import Rumor.Parser.Common (hspace)
-- >>> let parseTest inner = Mega.parseTest (inner <* Mega.eof)

{-| Parses an unquoted string followed by an indented multi-line one and an
  optional label.

  An unquoted string can be a single line.

  >>> parseTest (unquoted (Mega.mkPos 1)) "Hello world!"
  (String "Hello world!",Nothing)

  >>> parseTest (unquoted (Mega.mkPos 1)) "Hello world! [label]"
  (String "Hello world!",Just (Label "label"))

  An unquoted string over multiple lines must indent all lines the same, except
  for the first line.

  >>> parseTest (unquoted (Mega.mkPos 1)) "foo\n  bar\n  baz"
  (String "foo bar baz",Nothing)

  >>> parseTest (unquoted (Mega.mkPos 1)) "foo\n  bar\n  baz [label]"
  (String "foo bar baz",Just (Label "label"))

  >>> parseTest (unquoted (Mega.mkPos 1)) "foo\n  bar\n  baz\n  [label]"
  (String "foo bar baz",Just (Label "label"))

  An unquoted string can start indented on the next line.

  >>> parseTest (unquoted (Mega.mkPos 1)) "\n  bar\n  baz"
  (String "bar baz",Nothing)

  >>> parseTest (unquoted (Mega.mkPos 1)) "\n  bar\n  baz\n  [label]"
  (String "bar baz",Just (Label "label"))

  >>> parseTest (unquoted (Mega.mkPos 1)) "\nbar\nbaz"
  2:1:
    |
  2 | bar
    | ^
  incorrect indentation (got 1, should be greater than 1)

  An unquoted string cannot be empty.

  >>> parseTest (unquoted (Mega.mkPos 1)) ""
  1:1:
    |
  1 | <empty line>
    | ^
  unexpected end of input
  expecting carriage return, crlf newline, newline, or unquoted line

  >>> parseTest (do hspace; unquoted (Mega.mkPos 1)) "  "
  1:3:
    |
  1 |
    |   ^
  unexpected end of input
  expecting carriage return, crlf newline, newline, or unquoted line

  >>> parseTest (do hspace; unquoted (Mega.mkPos 1)) "  \n"
  2:1:
    |
  2 | <empty line>
    | ^
  unexpected end of input
  expecting unquoted line

  >>> parseTest (unquoted (Mega.mkPos 1)) "\n  "
  2:3:
    |
  2 |
    |   ^
  unexpected end of input
  expecting unquoted line

  Trailing vertical and horizontal space is consumed.

  >>> parseTest (unquoted (Mega.mkPos 1)) "foo\n"
  (String "foo",Nothing)

  >>> parseTest (unquoted (Mega.mkPos 1)) "foo  \n"
  (String "foo",Nothing)

  >>> parseTest (unquoted (Mega.mkPos 1)) "foo  \n  bar\t\n  baz\n"
  (String "foo bar baz",Nothing)

  >>> parseTest (unquoted (Mega.mkPos 1)) "foo  \n  bar\t\n  baz  "
  (String "foo bar baz",Nothing)

  >>> parseTest (unquoted (Mega.mkPos 1)) "foo  \n  bar\t\n  baz  \n"
  (String "foo bar baz",Nothing)

  >>> parseTest (unquoted (Mega.mkPos 1)) "foo  \n  bar\t\n  baz  \n  "
  (String "foo bar baz",Nothing)
-}
unquoted :: Mega.Pos -> Parser (Rumor.Expression Text, Maybe Rumor.Label)
unquoted ref = do
  emptyLine <- Mega.try (do eolf; pure True) <|> pure False

  literal <-
    if emptyLine
    then do
      -- If the first line is empty, then an indented block is required
      space
      endOfFile <- Mega.try (do Mega.eof; pure True) <|> pure False

      if endOfFile
      then unquotedLine <?> "unquoted line" -- Used only for the error message
      else do
        _ <- Lexer.indentGuard space GT ref
        Mega.try unquotedBlock

    else do
      first <- unquotedLine <?> "unquoted line"

      -- If the first line exists, then a block is optional
      space
      actual <- Lexer.indentLevel
      block <-
        if actual > ref
        then Mega.optional unquotedBlock
        else pure Nothing

      case block of
        Just rest -> pure (first <> Rumor.String " " <> rest)
        Nothing -> pure first

  space
  actual <- Lexer.indentLevel
  label <-
    if actual > ref
    then Mega.optional Identifier.label
    else pure Nothing
  space
  pure (literal, label)

{-| Parses an interpolated, non-empty, multi-line string literal.

  Blocks can contain a single string literal.

  >>> parseTest unquotedBlock "Hello world!"
  String "Hello world!"

  >>> parseTest unquotedBlock "I have { 5 } mangoes!"
  Concat (String "I have ") (Concat (NumberToString (Number 5.0)) (String " mangoes!"))

  >>> parseTest unquotedBlock "Hello\\nworld!"
  String "Hello\nworld!"

  Blocks can have multiple indented lines, as long as each line has the same
  amount of indentation. Each line will be joined with a single space.

  >>> parseTest unquotedBlock "Hello\nworld!"
  String "Hello world!"

  >>> parseTest (do hspace; unquotedBlock) "  foo\n  bar\n  baz"
  String "foo bar baz"

  >>> parseTest (do hspace; unquotedBlock) "  foo\r\n  bar\r\n  baz"
  String "foo bar baz"

  >>> parseTest (do hspace; unquotedBlock) "  foo\n  bar\n baz"
  2:6:
    |
  2 |   bar
    |      ^
  unexpected newline
  expecting '\', end of input, interpolation, literal char, or non-space literal char

  Blocks can also end when a label is encountered

  >>> parseTest unquotedBlock "Hello world! [label]"
  1:14:
    |
  1 | Hello world! [label]
    |              ^
  unexpected '['
  expecting '\', end of input, interpolation, or literal char

  >>> parseTest unquotedBlock "foo\nbar\nbaz [label]"
  3:5:
    |
  3 | baz [label]
    |     ^
  unexpected '['
  expecting '\', end of input, interpolation, or literal char

  >>> parseTest (do hspace; unquotedBlock) "  foo\n  bar\n    baz"
  3:5:
    |
  3 |     baz
    |     ^
  incorrect indentation (got 5, should be equal to 3)

  Trailing horizontal space is consumed on each line, but not the vertical space
  on the last line.

  >>> parseTest unquotedBlock "foo  \nbar\t\nbaz  "
  String "foo bar baz"

  >>> parseTest unquotedBlock "foo  \nbar  \nbaz  \n"
  3:6:
    |
  3 | baz  
    |      ^
  unexpected newline
  expecting '\', end of input, interpolation, or literal char
-}
unquotedBlock :: Parser (Rumor.Expression Text)
unquotedBlock = do
  ref <- Lexer.indentLevel

  let
    indentedUnquotedLine = do
      _ <- Lexer.indentGuard space EQ ref
      unquotedLine <?> "unquoted string"
    unindentedOrEmpty =
      Mega.lookAhead do
        space
        actual <- Lexer.indentLevel
        if actual < ref
        then pure ()
        else
          if actual == ref
          then eolf
          else
            Lexer.incorrectIndent LT ref actual
        pure ()
    startBracket =
      Mega.lookAhead do
        space
        _ <- Char.char '['
        pure ()

  texts <-
    Mega.someTill
      indentedUnquotedLine
      (     Mega.try startBracket
        <|> Mega.try unindentedOrEmpty
        <|> Mega.eof
      )

  pure
    ( mconcat
        ( List.intersperse
            (Rumor.String " ")
            (List.filter (/= mempty) texts)
        )
    )

{-| Parses an unquoted line, which is an interpolated, non-empty string literal.

  >>> parseTest unquotedLine "Hello world!"
  String "Hello world!"

  >>> parseTest unquotedLine "I have { 5 } mangoes!"
  Concat (String "I have ") (Concat (NumberToString (Number 5.0)) (String " mangoes!"))

  Unquoted lines end whenever vertical space or a label is enountered, without
  consuming the vertical space or label.

  >>> parseTest unquotedLine "Hello\nworld!"
  1:6:
    |
  1 | Hello
    |      ^
  unexpected newline
  expecting '\', end of input, interpolation, literal char, or non-space literal char

  >>> parseTest unquotedLine "Hello world! [label]"
  1:14:
    |
  1 | Hello world! [label]
    |              ^
  unexpected '['
  expecting '\', end of input, interpolation, or literal char

  You can escape characters like you can in a string expression.

  >>> parseTest unquotedLine "Hello! \\[not a label\\]"
  String "Hello! [not a label]"

  >>> parseTest unquotedLine "Hello\\nworld!"
  String "Hello\nworld!"

  Horizontal space between words is maintained.

  >>> parseTest unquotedLine "Hello   world!"
  String "Hello   world!"

  Trailing horizontal space is consumed, but not vertical space.

  >>> parseTest unquotedLine "Hello world!  "
  String "Hello world!"

  >>> parseTest unquotedLine "Hello world!\t"
  String "Hello world!"

  >>> parseTest unquotedLine "Hello world!\n"
  1:13:
    |
  1 | Hello world!
    |             ^
  unexpected newline
  expecting '\', end of input, interpolation, or literal char

  Unquoted lines cannot be empty.

  >>> parseTest unquotedLine ""
  1:1:
    |
  1 | <empty line>
    | ^
  unexpected end of input
  expecting '\', interpolation, or non-space literal char

  >>> parseTest unquotedLine "  "
  1:1:
    |
  1 |
    | ^
  unexpected space
  expecting '\', interpolation, or non-space literal char
-}
unquotedLine :: Parser (Rumor.Expression Text)
unquotedLine = do
  let
    end = (do _ <- Char.char '['; pure ()) <|> eolf
    stripEnd literal = do
      -- Strip the end of the text if this is at the end of the line
      next <- Mega.lookAhead ((do end; pure True) <|> pure False)
      if next
      then pure (Rumor.String (T.stripEnd literal))
      else pure (Rumor.String literal)

    nonSpaceLiteralString = do
      literal <-
        Mega.takeWhile1P
          (Just "non-space literal char")
          (\ch -> ch `notElem` (fst <$> unquotedEscapes) && not (isSpace ch))
      stripEnd literal
    literalString = do
      literal <-
        Mega.takeWhile1P
          (Just "literal char")
          (`notElem` (fst <$> unquotedEscapes))
      stripEnd literal

  first <- nonSpaceLiteralString
      <|> Expression.escape unquotedEscapes
      <|> Expression.interpolation

  rest <- Mega.many
    (     literalString
      <|> Expression.escape unquotedEscapes
      <|> Expression.interpolation
    )

  pure (mconcat (first:rest))

unquotedEscapes :: [(Char, Text)]
unquotedEscapes =
  [ ('[', "[")
  , (']', "]")
  ] <> Expression.stringEscapes
