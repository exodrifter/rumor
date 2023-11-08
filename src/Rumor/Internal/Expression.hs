{-# LANGUAGE RankNTypes #-}
module Rumor.Internal.Expression
( Expression(..)
, concatStrings
) where

import Data.Text (Text)
import Data.Scientific (Scientific)
import Rumor.Internal.VariableName (VariableName)

import qualified Data.List as List

-- $setup
-- >>> import Data.Either (fromRight)
-- >>> import Data.NonEmptyText as NET
-- >>> import Rumor.Internal
-- >>> import Rumor.Internal.Unicode
-- >>> import Rumor.Internal.VariableName
-- >>> import Rumor.Parser.Common
--
-- >>> :{
-- let setVariableTypes c0 = do
--       c1 <- setVariableType (VariableName (Unicode (NET.new 's' "tring"))) StringType c0
--       c2 <- setVariableType (VariableName (Unicode (NET.new 'n' "umber"))) NumberType c1
--       c3 <- setVariableType (VariableName (Unicode (NET.new 'b' "oolean"))) BooleanType c2
--       pure c3
-- :}
--
-- >>> let context = fromRight undefined (setVariableTypes newContext)
-- >>> let foo = Variable (VariableName (Unicode (NET.new 'f' "oo")))
-- >>> let bar = Variable (VariableName (Unicode (NET.new 'b' "ar")))
-- >>> let string = Variable (VariableName (Unicode (NET.new 's' "tring")))
-- >>> let number = Variable (VariableName (Unicode (NET.new 'n' "umber")))
-- >>> let boolean = Variable (VariableName (Unicode (NET.new 'b' "oolean")))

{-| When parsing, Rumor will parse expressions into this type, which is an
  expression which is loosely typed -- the types of the variables are not known.

  This is lets us do the parsing step separately from the type inference step.
-}
data Expression =
  -- Boolean
    Boolean Bool
  | LogicalNot Expression
  | LogicalAnd Expression Expression
  | LogicalOr Expression Expression
  | LogicalXor Expression Expression

  -- Number
  | Number Scientific
  | Addition Expression Expression
  | Subtraction Expression Expression
  | Multiplication Expression Expression
  | Division Expression Expression

  -- String
  | String Text
  | Concat Expression Expression

  -- Overloaded
  | Variable VariableName
  | Equal Expression Expression
  | NotEqual Expression Expression
  | ToString Expression
  deriving (Eq, Show)

concatStrings :: [Expression] -> Expression
concatStrings expressions =
  let
    step lhs rhs =
      case (lhs, rhs) of
        (String l, String r) ->
          String (l <> r)
        (String l, Concat (String r1) r2) ->
          Concat (String (l <> r1)) r2
        (Concat l1 (String l2), String r) ->
          Concat l1 (String (l2 <> r))
        (Concat l1 (String l2), Concat (String r1) r2) ->
          Concat l1 (Concat (String (l2 <> r1)) r2)
        (l, r) -> Concat l r
  in
    case List.filter ((/=) (String "")) expressions of
      [] -> String ""
      [expression] -> expression
      _ -> List.foldr1 step expressions
