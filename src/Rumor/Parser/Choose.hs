module Rumor.Parser.Choose
( choose
) where

import Rumor.Parser.Common (Parser, hlexeme)

import qualified Rumor.Internal as Rumor

-- $setup
-- >>> import Rumor.Parser.Common
-- >>> import Rumor.Internal
-- >>> let parse inner = parseTest newContext (inner <* eof)

{-| Parses a choose command.

  >>> parse choose "choose"
  Choose
-}
choose :: Parser Rumor.Node
choose = do
  _ <- hlexeme "choose"
  pure Rumor.Choose
