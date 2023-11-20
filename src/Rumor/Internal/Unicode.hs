module Rumor.Internal.Unicode
( Unicode(..)
, unicodeToNET
, unicodeToText
) where

import Data.NonEmptyText (NonEmptyText)
import Data.Text (Text)

import qualified Data.NonEmptyText as NET
import qualified Data.Text.ICU.Normalize2 as Normalize

-- $setup
-- >>> import qualified Data.NonEmptyText as NET

{-| A non-empty unicode string. The string is converted to a normal form when
  testing for equality, but is stored as it was originally written by the
  user.

  >>> Unicode (NET.new 'f' "oo") == Unicode (NET.new 'f' "oo")
  True

  >>> Unicode (NET.new 'f' "oo") == Unicode (NET.new 'b' "ar")
  False

  >>> Unicode (NET.singleton '\225') == Unicode (NET.new 'a' "\769")
  True

  >>> let unwrap (Unicode net) = net
  >>> unwrap (Unicode (NET.singleton '\225')) == unwrap (Unicode (NET.new 'a' "\769"))
  False
-}
newtype Unicode = Unicode NonEmptyText
  deriving Show

instance Eq Unicode where
  (Unicode l) == (Unicode r) =
    Normalize.compareUnicode' (NET.toText l) (NET.toText r) == EQ

instance Ord Unicode where
  compare (Unicode l) (Unicode r) =
    Normalize.compareUnicode' (NET.toText l) (NET.toText r)

unicodeToNET :: Unicode -> NonEmptyText
unicodeToNET (Unicode net) = net

unicodeToText :: Unicode -> Text
unicodeToText = NET.toText . unicodeToNET
