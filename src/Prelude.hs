{-# LANGUAGE NoImplicitPrelude #-}

module Prelude
( Data.Bool.Bool(..)
, Data.Char.Char
, Data.Either.Either(..)
, Data.Eq.Eq(..)
, Data.Fixed.Fixed(..)
, Data.Fixed.HasResolution
, Data.Maybe.Maybe(..)
, Data.Ord.Ord
, Data.String.IsString
, System.IO.FilePath
, System.IO.IO
, Text.Show.Show(..)

-- Functions
, Control.Applicative.pure
, Control.Monad.fail
, Control.Monad.return
, Control.Monad.void
, Data.Either.isLeft
, Data.Functor.fmap
, Data.List.elem
, Text.Read.readMaybe

-- Operators
, (Control.Applicative.*>)
, (Control.Applicative.<$>)
, (Control.Applicative.<*)
, (Control.Applicative.<|>)
, (Data.Bool.&&)
, (Data.Bool.||)
, (Data.Function.$)
, (Data.Function..)
, (Data.List.++)
, (Data.Monoid.<>)
, (GHC.Num.*)
, (GHC.Num.+)
, (GHC.Num.-)
, (GHC.Real./)
) where

import qualified Control.Applicative
import qualified Control.Monad
import qualified Data.Bool
import qualified Data.Char
import qualified Data.Either
import qualified Data.Eq
import qualified Data.Fixed
import qualified Data.Function
import qualified Data.Functor
import qualified Data.List
import qualified Data.Maybe
import qualified Data.Monoid
import qualified Data.Ord
import qualified Data.String
import qualified GHC.Num
import qualified GHC.Real
import qualified System.IO
import qualified Text.Read
import qualified Text.Show
