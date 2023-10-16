module Rumor where

import Data.Functor (($>))
import Data.Scientific (Scientific)
import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec ((<?>), (<|>))

import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Text as T
import qualified Text.Megaparsec as Mega
import qualified Text.Megaparsec.Char as Char
import qualified Text.Megaparsec.Char.Lexer as Lexer
import qualified Text.Megaparsec.Error as Error
import qualified Text.Parser.Combinators as Combinators
import qualified Rumor.Internal.Types as Rumor

type Parser a = Mega.Parsec Void Text a

parse :: String -> T.Text -> Either String [Rumor.Node]
parse fileName fileContents =
  case Mega.runParser parser fileName fileContents of
    Right result -> Right result
    Left err -> Left (Error.errorBundlePretty err)

parser :: Parser [Rumor.Node]
parser =
  Mega.manyTill (hlexeme node <* space) Mega.eof

node :: Parser Rumor.Node
node =
      Mega.try say
  <|> Mega.try add
  <|> action

say :: Parser Rumor.Node
say =
  hlexeme (dialog ':' Rumor.Say)

add :: Parser Rumor.Node
add =
  hlexeme (dialog '+' Rumor.Add)

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
      speaker <- hlexeme (Mega.optional identifier)
      _ <- hlexeme (Char.char sep)
      firstText <- unquotedLine
      pure
        ( Lexer.IndentMany
            Nothing
            (\texts -> pure (mkDialog speaker (firstText:texts)))
            unquotedLine
        )

  in
    Lexer.indentBlock space dialogIndent

action :: Parser Rumor.Node
action = do
  actionName <- hlexeme identifier
  _ <- lexeme (Char.char '(')

  result <-
        Mega.try (lexeme (action4 actionName))
    <|> Mega.try (lexeme (action3 actionName))
    <|> Mega.try (lexeme (action2 actionName))
    <|> Mega.try (lexeme (action1 actionName))
    <|> pure (Rumor.Action0 actionName)

  _ <- Char.char ')' <?> "end parenthesis"
  pure result

action1 :: Text -> Parser Rumor.Node
action1 actionName = do
  param1 <- textExpression
  pure (Rumor.Action1 actionName param1)

action2 :: Text -> Parser Rumor.Node
action2 actionName = do
  param1 <- lexeme textExpression
  _ <- lexeme (Char.char ',')
  param2 <- textExpression
  pure (Rumor.Action2 actionName param1 param2)

action3 :: Text -> Parser Rumor.Node
action3 actionName = do
  param1 <- lexeme textExpression
  _ <- lexeme (Char.char ',')
  param2 <- lexeme textExpression
  _ <- lexeme (Char.char ',')
  param3 <- textExpression
  pure (Rumor.Action3 actionName param1 param2 param3)

action4 :: Text -> Parser Rumor.Node
action4 actionName = do
  param1 <- lexeme textExpression
  _ <- lexeme (Char.char ',')
  param2 <- lexeme textExpression
  _ <- lexeme (Char.char ',')
  param3 <- lexeme textExpression
  _ <- lexeme (Char.char ',')
  param4 <- textExpression
  pure (Rumor.Action4 actionName param1 param2 param3 param4)

--------------------------------------------------------------------------------
-- Expression
--------------------------------------------------------------------------------

interpolation :: Parser (Rumor.Expression Text)
interpolation = do
  _ <- lexeme (Char.char '{')

  text <-
        Mega.try (Rumor.BooleanToString <$> booleanExpression)
    <|> Mega.try (Rumor.NumberToString <$> numberExpression)
    <|> lexeme textExpression

  _ <- Char.char '}' <?> "end interpolation"
  pure text

textExpression :: Parser (Rumor.Expression Text)
textExpression = do
  _ <- Char.char '"'
  text <- unquotedLine
  _ <- Char.char '"' <?> "end double quote"
  pure text

numberExpression :: Parser (Rumor.Expression Scientific)
numberExpression =
  let
    lit = Rumor.Number <$> Lexer.signed hspace Lexer.scientific

    expr   = term `Combinators.chainl1` addop
    term   = factor `Combinators.chainl1` mulop
    factor = lexeme (parenthesis expr <|> lit)

    mulop  = Rumor.Multiply <$ lexeme (Char.char '*')
         <|> Rumor.Divide <$
                ( Mega.try do
                    _ <- lexeme (char '/')
                    Mega.try (Mega.notFollowedBy (char '='))
                )

    addop  = Rumor.Addition <$ lexeme (Char.char '+')
         <|> Rumor.Subtraction <$ lexeme (Char.char '-')

  in
    Rumor.simplify <$> expr

booleanExpression :: Parser (Rumor.Expression Bool)
booleanExpression =
  let
    lit = Rumor.Boolean <$> lexeme
      (Char.string "true" $> True <|> Char.string "false" $> False)

    expr = andTerm `Combinators.chainl1` orOp
    andTerm = xorTerm `Combinators.chainl1` andOp
    xorTerm = neqTerm `Combinators.chainl1` xorOp
    neqTerm = eqTerm `Combinators.chainl1` neqOp
    eqTerm = factor `Combinators.chainl1` eqOp
    factor = lexeme (parenthesis expr <|> equalities <|> notOp lit <|> lit)

    notOp inner = do
      _ <- lexeme (Char.string "!" <|> Char.string "not")
      Rumor.LogicalNot <$> inner
    eqOp = Rumor.EqualBoolean <$ lexeme (Char.string "==")
    neqOp = Rumor.NotEqualBoolean <$ lexeme (Char.string "/=")
      <|> Rumor.NotEqualBoolean <$ lexeme (Char.string "!=")
    xorOp = Rumor.LogicalXor <$ lexeme (Char.string "^")
      <|> Rumor.LogicalXor <$ lexeme (Char.string "xor")
    andOp = Rumor.LogicalAnd <$ lexeme (Char.string "&&")
      <|> Rumor.LogicalAnd <$ lexeme (Char.string "and")
    orOp = Rumor.LogicalOr <$ lexeme (Char.string "||")
      <|> Rumor.LogicalOr <$ lexeme (Char.string "or")

    equalities =
          Mega.try (valueEquality textExpression Rumor.EqualString Rumor.NotEqualString)
      <|> valueEquality numberExpression Rumor.EqualNumber Rumor.NotEqualNumber

    valueEquality :: Show a => Parser a
                  -> (a -> a -> Rumor.Expression Bool)
                  -> (a -> a -> Rumor.Expression Bool)
                  -> Parser (Rumor.Expression Bool)
    valueEquality arg eqCons neqCons = do
      l <- lexeme arg
      f <- eqCons <$ lexeme (Char.string "==")
        <|> neqCons <$ lexeme (Char.string "/=")
        <|> neqCons <$ lexeme (Char.string "!=")
        
      r <- lexeme arg
      pure (f l r)

  in
    Rumor.simplify <$> expr

unquotedLine :: Parser (Rumor.Expression Text)
unquotedLine = do
  let escapedChar = do
        _ <- Char.char '\\'
        ch <- Char.char 'n' $> "\n"
          <|> Char.char 'r' $> "\r"
          <|> Char.char '\\' $> "\\"
          <|> Char.char '{' $> "{"
          <|> Char.char '}' $> "}"
          <|> Char.char '"' $> "\""
        pure (Rumor.String ch)

      literalString = do
        literal <-
          Mega.takeWhile1P
            (Just "literal char")
            (`notElem` ['\n', '\r', '\\', '{', '"'])
        -- Strip the end of the text if this is at the end of the line
        next <- Mega.lookAhead
          (Mega.optional (Char.char '\n' <|> Char.char '\r'))
        if next `elem` (Just <$> ['\n', '\r'])
        then pure (Rumor.String (T.stripEnd literal))
        else pure (Rumor.String literal)

  text <- Mega.many (literalString <|> escapedChar <|> interpolation)
  pure (mconcat text)

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

identifier :: Parser Text
identifier =
  Mega.takeWhile1P
    (Just "letter, mark, or digit character")
    (\char -> Char.isLetter char || Char.isMark char || Char.isDigit char)

line :: Parser Text
line =
  Mega.takeWhile1P Nothing (`notElem` ['\n', '\r'])

lexeme :: Parser a -> Parser a
lexeme = Lexer.lexeme space

hlexeme :: Parser a -> Parser a
hlexeme = Lexer.lexeme hspace

space :: Parser ()
space = Lexer.space Char.space1 lineComment blockComment

hspace :: Parser ()
hspace = Lexer.space Char.hspace1 lineComment blockComment

lineComment :: Parser ()
lineComment = Lexer.skipLineComment "//"

blockComment :: Parser ()
blockComment = Lexer.skipBlockComment "/*" "*/"

parenthesis :: Parser a -> Parser a
parenthesis inner = do
  _ <- lexeme (Char.char '(')
  result <- lexeme inner
  _ <- Char.char ')' <?> "end parenthesis"
  pure result
