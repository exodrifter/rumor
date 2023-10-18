module Rumor.Parser.Unquoted
( unquotedBlock
, unquotedLine
) where

import Data.Text (Text)
import Rumor.Parser.Common (Parser, (<|>))

import qualified Data.List as List
import qualified Data.Text as T
import qualified Rumor.Internal.Types as Rumor
import qualified Rumor.Parser.Expression as Expression
import qualified Rumor.Parser.Identifier as Identifier
import qualified Rumor.Parser.Lexeme as Lexeme
import qualified Text.Megaparsec as Mega
import qualified Text.Megaparsec.Char as Char
import qualified Text.Megaparsec.Char.Lexer as Lexer

-- $setup
-- >>> import qualified Text.Megaparsec as Mega
-- >>> let parseTest inner = Mega.parseTest (inner <* Mega.hidden Mega.eof)

{-| Parses an unquoted block, which contains an identifying starting parser
  followed by multiple indented unquoted string literals on the same or
  following lines. An unquoted line will have all trailing horizontal whitespace
  on each line removed.

  >>> let block = unquotedBlock ">" (const id)
  >>> parseTest block "> Hello world!"
  String "Hello world!"

  >>> parseTest block "> I have { 5 } mangoes!"
  Concat (String "I have ") (Concat (NumberToString (Number 5.0)) (String " mangoes!"))

  >>> parseTest block "> Hello\\nworld!"
  Concat (String "Hello") (Concat (String "\n") (String "world!"))

  >>> parseTest block "> Hello\n world!" -- Indented blocks
  Concat (String "Hello") (Concat (String " ") (String "world!"))

  Whitespace handling:
  >>> parseTest block "> Hello world!   " -- Trailing whitespace is removed
  String "Hello world!"

  >>> parseTest block "> Hello  \n world!  " -- Trailing whitespace is removed
  Concat (String "Hello") (Concat (String " ") (String "world!"))
-}
unquotedBlock ::
  Parser a ->
  (a -> Rumor.Expression Text -> b) ->
  Parser b
unquotedBlock front constructor = do
  -- Make sure we aren't indented
  _ <- Lexer.indentGuard Lexeme.space EQ =<< Lexer.indentLevel

  let
    combine texts =
      mconcat
        ( List.intersperse
            (Rumor.String " ")
            (List.filter (/= mempty) texts)
        )

    indent = do
      result <- Lexeme.hlexeme front
      (firstText, _) <- unquotedLine -- TODO
      pure
        ( Lexer.IndentMany
            Nothing
            (\texts -> pure (constructor result (combine (firstText:texts))))
            (fst <$> unquotedLine)
        )
  Lexer.indentBlock Lexeme.space indent

{-| Parses an unquoted line, which is an unquoted string literal which may end
  with a label. An unquoted line will have all trailing horizontal whitespace
  removed.

  >>> parseTest unquotedLine "Hello world!"
  (String "Hello world!",Nothing)

  >>> parseTest unquotedLine "[label]" -- Just a label is okay too
  (String "",Just (Label "label"))

  >>> parseTest unquotedLine "Hello world! [label]"
  (String "Hello world!",Just (Label "label"))

  >>> parseTest unquotedLine "Hello world! \\[label\\]" -- You can escape brackets
  (Concat (String "Hello world! ") (Concat (String "[") (Concat (String "label") (String "]"))),Nothing)

  >>> parseTest unquotedLine "I have { 5 } mangoes!"
  (Concat (String "I have ") (Concat (NumberToString (Number 5.0)) (String " mangoes!")),Nothing)

  >>> parseTest unquotedLine "Hello\\nworld!"
  (Concat (String "Hello") (Concat (String "\n") (String "world!")),Nothing)

  Whitespace handling:
  >>> parseTest unquotedLine "Hello world!   " -- Trailing whitespace is removed
  (String "Hello world!",Nothing)

  >>> parseTest unquotedLine "Hello world!   [label]   " -- Trailing whitespace is removed
  (String "Hello world!",Just (Label "label"))

  >>> parseTest unquotedLine "Hello\nworld!" -- Newlines are not okay
  1:6:
    |
  1 | Hello
    |      ^
  unexpected newline
  expecting '\', interpolation, label, or literal char

  Error example:
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
    eol =
          (do _ <- Char.char '\n'; pure (Right ()))
      <|> (do _ <- Char.char '\r'; pure (Right ()))
      <|> (do _ <- Char.char '['; pure (Right ()))
      <|> (do Mega.eof; pure (Right ()))
      <|> pure (Left ())

    literalString = do
      literal <-
        Mega.takeWhile1P
          (Just "literal char")
          (`notElem` (fst <$> unquotedEscapes))

      -- Strip the end of the text if this is at the end of the line
      next <- Mega.lookAhead eol
      if next == Right ()
      then pure (Rumor.String (T.stripEnd literal))
      else pure (Rumor.String literal)

  text <- Mega.many
    (     literalString
      <|> Expression.escape unquotedEscapes
      <|> Expression.interpolation
    )

  label <- Mega.optional (Lexeme.hlexeme Identifier.label)
  pure (mconcat text, label)

unquotedEscapes :: [(Char, Text)]
unquotedEscapes =
  [ ('[', "[")
  , (']', "]")
  ] <> Expression.stringEscapes
