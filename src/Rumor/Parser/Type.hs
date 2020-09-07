module Rumor.Parser.Type
( Parser(..)
, Parsec.ParseError
, Parsec.SourceName
, runParser

-- User State
, ParserState
, getRandoms
, getUsedIdentifiers
, setUsedIdentifiers
) where

import Rumor.Object

import Control.Applicative (Applicative(..), Alternative(..))
import Control.Monad (Monad(..), MonadFail(..))
import Data.Functor (Functor(..))
import System.Random (Random(..))
import Data.Set (Set)
import qualified Data.Hashable as Hashable
import qualified Data.List as List
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified System.Random as Random
import qualified Text.Parsec as Parsec
import qualified Text.Parsec.Indent as Parsec

-- A parser that automatically rolls back when it fails, like
-- attoparsec.
newtype Parser a =
  Parser
    { unParser :: Parsec.IndentParser T.Text ParserState a
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
runParser p s t =
  let state =
        ParserState
          { usedIdentifiers = Set.empty
          , pureGen = Random.mkStdGen (Hashable.hash t)
          }
  in  Parsec.runIndentParser (unParser p) state s t

--------------------------------------------------------------------------------
-- User State
--------------------------------------------------------------------------------

data ParserState =
  ParserState
    { pureGen :: Random.StdGen
    , usedIdentifiers :: Set Identifier
    }

gets :: (ParserState -> a) -> Parser a
gets fn = Parser $ fn <$> Parsec.getState

modify :: (ParserState -> ParserState) -> Parser ()
modify = Parser . Parsec.modifyState

getRandom :: Random a => Parser a
getRandom = do
  (a, g) <- Random.random <$> gets pureGen
  modify (\s -> s { pureGen = g })
  pure a

getRandoms :: Random a => Int -> Parser [a]
getRandoms l = sequence $ List.replicate l getRandom

getUsedIdentifiers :: Parser (Set Identifier)
getUsedIdentifiers = gets usedIdentifiers

setUsedIdentifiers :: Set Identifier -> Parser ()
setUsedIdentifiers a = modify (\s -> s { usedIdentifiers = a })
