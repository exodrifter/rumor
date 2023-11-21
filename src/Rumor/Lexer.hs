{-# LANGUAGE RecordWildCards #-}

{-| This module contains the logic for lexing Rumor files. -}
module Rumor.Lexer
( tokenize
) where

import Prelude hiding (String)
import Data.Void (Void)
import Data.Text (Text)
import Data.Scientific (Scientific)
import Text.Megaparsec ((<?>))

import qualified Text.Megaparsec as Mega
import qualified Text.Megaparsec.Char as Char
import qualified Text.Megaparsec.Char.Lexer as Lexer
import qualified Text.Megaparsec.Error as Error

-- $setup
-- >>> import Text.Pretty.Simple (pPrint)

data ExpressionToken =
  -- Literals
    Boolean Bool
  | Number Scientific
  | String (Located StringToken)

  -- Operators
  | And
  | Or
  | Xor
  | Addition
  | Subtraction
  | Multiplication
  | Division
  | Equal
  | NotEqual
  | LessThan
  | LessThanOrEqual
  | GreaterThan
  | GreaterThanOrEqual

  -- Delimiters
  | OpenParenthesis
  | CloseParenthesis
  deriving (Eq, Show)

data StringToken =
  StringToken
    { prefix :: Text
    , suffix :: Maybe ([Located ExpressionToken], Located StringToken)
    }
  deriving (Eq, Show)

data Located token =
  Located
    { start :: Int
    , token :: token
    }
  deriving (Eq, Show)

{-| Converts text into a list of tokens for use in a Rumor parser.

  >>> pPrint (tokenize "" "3 + 4 * (-5 + -6)")
  Right
      [ Located
          { start = 0
          , token = Number 3.0
          }
      , Located
          { start = 2
          , token = Addition
          }
      , Located
          { start = 4
          , token = Number 4.0
          }
      , Located
          { start = 6
          , token = Multiplication
          }
      , Located
          { start = 8
          , token = OpenParenthesis
          }
      , Located
          { start = 9
          , token = Number
              ( -5.0 )
          }
      , Located
          { start = 12
          , token = Addition
          }
      , Located
          { start = 14
          , token = Number
              ( -6.0 )
          }
      , Located
          { start = 16
          , token = CloseParenthesis
          }
      ]

  >>> pPrint (tokenize "" "\"{ \"foo\" + 1 } foo { 2 + \"bar\" } bar \"")
  Right
      [ Located
          { start = 0
          , token = String
              ( Located
                  { start = 0
                  , token = StringToken
                      { prefix = ""
                      , suffix = Just
                          (
                              [ Located
                                  { start = 3
                                  , token = String
                                      ( Located
                                          { start = 3
                                          , token = StringToken
                                              { prefix = "foo"
                                              , suffix = Nothing
                                              }
                                          }
                                      )
                                  }
                              , Located
                                  { start = 9
                                  , token = Addition
                                  }
                              , Located
                                  { start = 11
                                  , token = Number 1.0
                                  }
                              ]
                          , Located
                              { start = 14
                              , token = StringToken
                                  { prefix = " foo "
                                  , suffix = Just
                                      (
                                          [ Located
                                              { start = 21
                                              , token = Number 2.0
                                              }
                                          , Located
                                              { start = 23
                                              , token = Addition
                                              }
                                          , Located
                                              { start = 25
                                              , token = String
                                                  ( Located
                                                      { start = 25
                                                      , token = StringToken
                                                          { prefix = "bar"
                                                          , suffix = Nothing
                                                          }
                                                      }
                                                  )
                                              }
                                          ]
                                      , Located
                                          { start = 32
                                          , token = StringToken
                                              { prefix = " bar "
                                              , suffix = Nothing
                                              }
                                          }
                                      )
                                  }
                              }
                          )
                      }
                  }
              )
          }
      ]

  >>> pPrint (tokenize "" "\" { \"{ \"foo\" } \" } \"")
  Right
      [ Located
          { start = 0
          , token = String
              ( Located
                  { start = 0
                  , token = StringToken
                      { prefix = " "
                      , suffix = Just
                          (
                              [ Located
                                  { start = 4
                                  , token = String
                                      ( Located
                                          { start = 4
                                          , token = StringToken
                                              { prefix = ""
                                              , suffix = Just
                                                  (
                                                      [ Located
                                                          { start = 7
                                                          , token = String
                                                              ( Located
                                                                  { start = 7
                                                                  , token = StringToken
                                                                      { prefix = "foo"
                                                                      , suffix = Nothing
                                                                      }
                                                                  }
                                                              )
                                                          }
                                                      ]
                                                  , Located
                                                      { start = 14
                                                      , token = StringToken
                                                          { prefix = " "
                                                          , suffix = Nothing
                                                          }
                                                      }
                                                  )
                                              }
                                          }
                                      )
                                  }
                              ]
                          , Located
                              { start = 18
                              , token = StringToken
                                  { prefix = " "
                                  , suffix = Nothing
                                  }
                              }
                          )
                      }
                  }
              )
          }
      ]
-}
tokenize :: [Char] -> Text -> Either [Char] [Located ExpressionToken]
tokenize name code =
  case Mega.parse (locatedExpressionTokens Mega.eof) name code of
    Left err ->
      Left (Error.errorBundlePretty err)
    Right tokens ->
      Right tokens

located :: Parser token -> Parser (Located token)
located tokenParser = do
  start <- Mega.getOffset
  token <- tokenParser
  pure Located {..}

locatedExpressionTokens :: Parser () -> Parser [Located ExpressionToken]
locatedExpressionTokens = Mega.manyTill (located expressionToken)

expressionToken :: Parser ExpressionToken
expressionToken =
  Mega.choice
    [ Boolean <$> lexeme boolean <?> "boolean"
    , Number <$> lexeme number <?> "number"
    , String <$> lexeme string <?> "text"

    , Mega.choice
        [ And <$ lexeme "&&"
        , Or <$ lexeme "||"
        , Xor <$ lexeme "^^"
        , Addition <$ lexeme "+"
        , Subtraction <$ lexeme "-"
        , Multiplication <$ lexeme "*"
        , Division <$ lexeme "/"
        , Equal <$ lexeme "=="
        , NotEqual <$ lexeme "/="
        , LessThan <$ lexeme "<"
        , LessThanOrEqual <$ lexeme "<="
        , GreaterThan <$ lexeme ">"
        , GreaterThanOrEqual <$ lexeme ">="
        ] <?> "operator"

    , Mega.choice
        [ OpenParenthesis <$ lexeme "("
        , CloseParenthesis <$ lexeme ")"
        ] <?> "parenthesis"
    ]

boolean :: Parser Bool
boolean =
  Mega.choice
    [ True <$ "true"
    , False <$ "false"
    ]

number :: Parser Scientific
number =
  Mega.try (Lexer.signed mempty Lexer.scientific)

string :: Parser (Located StringToken)
string = do
  let
    go = do
      prefix <- Mega.takeWhileP Nothing (\c -> not (c `elem` ['{', '\"']))

      endQuote <- Mega.choice
        [ True <$ Char.char '\"'
        , False <$ lexeme (Char.char '{')
        ]
      suffix <-
        if endQuote
        then do
          pure Nothing
        else do
          interpolation <- locatedExpressionTokens (() <$ Char.char '}')
          start <- Mega.getOffset
          token <- go
          pure (Just (interpolation, Located {..}))

      pure (StringToken {..})

  startOfString <- Mega.getOffset
  _ <- Char.char '\"'
  Located startOfString <$> go

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

type Parser a = Mega.Parsec Void Text a

lexeme :: Parser a -> Parser a
lexeme = Lexer.lexeme space

space :: Parser ()
space = Lexer.space Char.space1 lineComment blockComment

lineComment :: Parser ()
lineComment = Lexer.skipLineComment "//"

blockComment :: Parser ()
blockComment = Lexer.skipBlockComment "/*" "*/"
