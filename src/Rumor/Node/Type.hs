module Rumor.Node.Type
( Node(..)
, Identifier(..)
) where

import Rumor.Expression.Type (Expression)

import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T

data Node r =
    Say (Maybe Identifier) (Expression r T.Text)
  | Append (Maybe Identifier) (Expression r T.Text)
  | Section Identifier (NE.NonEmpty (Node r))
  deriving stock (Eq, Show)

newtype Identifier = Identifier { unIdentifier :: T.Text }
  deriving stock (Eq, Show)
  deriving newtype (IsString)
