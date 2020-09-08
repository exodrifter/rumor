module Rumor.Parser.Combinator
( chainl1
, many
, many1
, manyTill
, notFollowedBy
, option
) where

import Rumor.Parser.Type (Parser(..))

import qualified Text.Parsec as Parsec

chainl1 :: Parser r a -> Parser r (a -> a -> a) -> Parser r a
chainl1 p op = Parser $ Parsec.chainl1 (unParser p) (unParser op)

many :: Parser r a -> Parser r [a]
many = Parser . Parsec.many . unParser

many1 :: Parser r a -> Parser r [a]
many1 = Parser . Parsec.many1 . unParser

manyTill :: Parser r a -> Parser r end -> Parser r [a]
manyTill p end =
  Parser $ Parsec.manyTill
    (unParser p)
    (Parsec.try . Parsec.lookAhead $ unParser end)

notFollowedBy :: (Show a) => Parser r a -> Parser r ()
notFollowedBy = Parser . Parsec.notFollowedBy . unParser

option :: Parser r a -> Parser r (Maybe a)
option p = Just <$> p <|> pure Nothing
