{-# LANGUAGE Safe #-}

module Rumor.Prelude
( Prelude.Bool(..)
, Prelude.Double
, Prelude.Eq(..)
, Prelude.Maybe(..)
, Prelude.Show(..)

-- Functions
, Prelude.fmap
, Prelude.not
, Prelude.notElem
, Prelude.pure
, Prelude.flip
, Prelude.fromIntegral

-- Operators
, (Control.Applicative.*>)
, (Control.Applicative.<$>)
, (Control.Applicative.<|>)
, (Prelude.$)
, (Prelude.&&)
, (Prelude.*)
, (Prelude.+)
, (Prelude.++)
, (Prelude.-)
, (Prelude..)
, (Prelude./)
, (Prelude.<>)
, (Prelude.||)
, (<||>)
) where

import Text.Parsec(ParsecT, try)
import qualified Control.Applicative
import qualified Prelude

-- | A custom combinator which implements choice like the Parsec alternative,
-- except that the parsers do not consume input if they fail.
(<||>) :: (ParsecT s u m a) -> (ParsecT s u m a) -> (ParsecT s u m a)
p1 <||> p2 = (Control.Applicative.<|>) (try p1) (try p2)

infixr 1 <||>
