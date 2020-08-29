module Rumor.Parser.Type
( Parser(..)
, Parsec.ParseError
, runParser
) where

import Control.Applicative (Applicative(..), Alternative(..))
import Control.Monad (Monad(..), MonadFail(..))
import Data.Functor (Functor(..))
import qualified Data.Text as T
import qualified Text.Parsec as Parsec
import qualified Text.Parsec.Indent as Parsec

-- A parser that automatically rolls back when it fails, like
-- attoparsec.
newtype Parser a =
  Parser
    { unParser :: Parsec.IndentParser T.Text () a
    }

instance Functor Parser where
  fmap fn p = Parser $ fmap fn (unParser p)

instance Applicative Parser where
  pure a = Parser $ pure a
  l <*> r = Parser $ unParser l <*> unParser r

instance Alternative Parser where
  empty = Parser empty
  l <|> r = Parser $ Parsec.try (unParser l) <|> Parsec.try (unParser r)

instance Monad Parser where
  return = pure
  p >>= f = Parser $ unParser p >>= (unParser . f)

instance MonadFail Parser where
  fail = Parser . Parsec.parserFail

runParser :: Parser a -> Parsec.SourceName -> T.Text -> Either Parsec.ParseError a
runParser p = Parsec.runIndentParser (unParser p) ()
