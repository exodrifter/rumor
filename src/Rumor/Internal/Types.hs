{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}

module Rumor.Internal.Types
  ( Speaker(..)
  , Node(..)
  , Expression(..)
  ) where

import Data.Text (Text)
import qualified Data.Text as T

-- | The identifier for a character who is saying something.
newtype Speaker = Speaker Text
  deriving (Eq, Show)

-- | The nodes represent the abstract syntax tree of a Rumor dialog.
data Node =
    Say (Maybe Speaker) (Expression Text)
  | Add (Maybe Speaker) (Expression Text)
  | Action0 Text
  | Action1 Text (Expression Text)
  | Action2 Text (Expression Text) (Expression Text)
  | Action3 Text (Expression Text) (Expression Text) (Expression Text)
  | Action4 Text (Expression Text) (Expression Text) (Expression Text) (Expression Text)
  deriving (Eq, Show)

-- | Represents expressions in a Rumor dialog.
data Expression typ where
  String :: Text -> Expression Text

deriving instance Eq (Expression a)
deriving instance Show (Expression a)

instance Semigroup (Expression Text) where
  String l <> String r = String (l <> r)

instance Monoid (Expression Text) where
  mempty = String ""

