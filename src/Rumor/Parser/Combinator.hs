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

chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainl1 p op = Parser $ Parsec.chainl1 (unParser p) (unParser op)

many :: Parser a -> Parser [a]
many = Parser . Parsec.many . unParser

many1 :: Parser a -> Parser [a]
many1 = Parser . Parsec.many1 . unParser

manyTill :: Parser a -> Parser end -> Parser [a]
manyTill p end =
  Parser $ Parsec.manyTill
    (unParser p)
    (Parsec.try . Parsec.lookAhead $ unParser end)

notFollowedBy :: (Show a) => Parser a -> Parser ()
notFollowedBy = Parser . Parsec.notFollowedBy . unParser

option :: Parser a -> Parser (Maybe a)
option p = Just <$> p <|> pure Nothing
