module Rumor.Parser.Type
( Parser(..)
, Parsec.ParseError
, Parsec.SourceName
, runParser

-- User State
, ParserState
, addSection
, getRandoms
, getSections
) where

import Rumor.Object

import Control.Applicative (Applicative(..), Alternative(..))
import Control.Monad (Monad(..), MonadFail(..))
import Data.Functor (Functor(..))
import GHC.Enum (maxBound)
import System.Random (Random(..))
import qualified Data.Hashable as Hashable
import qualified Data.List as List
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified System.Random as Random
import qualified Text.Parsec as Parsec
import qualified Text.Parsec.Indent as Parsec

-- A parser that automatically rolls back when it fails, like
-- attoparsec.
newtype Parser r a =
  Parser
    { unParser :: Parsec.IndentParser T.Text (ParserState r) a
    }

instance Functor (Parser r) where
  fmap fn p = Parser $ fmap fn (unParser p)

instance Applicative (Parser r) where
  pure a = Parser $ pure a
  l <*> r = Parser $ unParser l <*> unParser r

instance Alternative (Parser r) where
  empty = Parser empty
  l <|> r = Parser $ Parsec.try (unParser l) <|> Parsec.try (unParser r)

instance Monad (Parser r) where
  return = pure
  p >>= f = Parser $ unParser p >>= (unParser . f)

instance MonadFail (Parser r) where
  fail = Parser . Parsec.parserFail

runParser :: Parser r a -> Parsec.SourceName -> T.Text -> Either Parsec.ParseError a
runParser p s t =
  let state =
        ParserState
          { pureGen = Random.mkStdGen (Hashable.hash t)
          , sections = Map.empty
          }
  in  Parsec.runIndentParser (unParser p) state s t

--------------------------------------------------------------------------------
-- User State
--------------------------------------------------------------------------------

data ParserState r =
  ParserState
    { pureGen :: Random.StdGen
    , sections :: Map Identifier (NE.NonEmpty (Node r))
    }

gets :: (ParserState r -> a) -> Parser r a
gets fn = Parser $ fn <$> Parsec.getState

modify :: (ParserState r -> ParserState r) -> Parser r ()
modify = Parser . Parsec.modifyState

addSection :: Identifier -> [Node r] -> Parser r ()
addSection i ns =
  case NE.nonEmpty ns of
    Nothing -> fail "labeled section does not contain anything"
    Just nodes -> do
      ss <- gets sections
      if Map.member i ss
      then fail "duplicate identifier label"
      else
        if Map.size ss >= (maxBound :: Int)
        then fail "cannot create any more sections"
        else modify (\s -> s { sections = Map.insert i nodes ss })

getRandom :: Random a => Parser r a
getRandom = do
  (a, g) <- Random.random <$> gets pureGen
  modify (\s -> s { pureGen = g })
  pure a

getRandoms :: Random a => Int -> Parser r [a]
getRandoms l = sequence $ List.replicate l getRandom

getSections :: Parser r (Map Identifier (NE.NonEmpty (Node r)))
getSections = gets sections
