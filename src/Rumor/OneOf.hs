module Rumor.OneOf
( OneOf(..)
, first
, second
, firsts
, seconds
) where

import qualified Data.Maybe as Maybe

data OneOf a b = First a | Second b

first :: OneOf a b -> Maybe a
first oneOf =
  case oneOf of
    First a -> Just a
    Second _ -> Nothing

second :: OneOf a b -> Maybe b
second oneOf =
  case oneOf of
    First _ -> Nothing
    Second b -> Just b

firsts :: [OneOf a b] -> [a]
firsts = Maybe.catMaybes . fmap first

seconds :: [OneOf a b] -> [b]
seconds = Maybe.catMaybes . fmap second
