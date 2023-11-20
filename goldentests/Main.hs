module Main where

import Data.Text (Text)

import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Rumor
import qualified Rumor.Internal as Rumor

data Interactive =
    Comment Text
  | Input Text

main :: IO ()
main = do
  let fileName = "goldentests/samples/action.rumor-interactive"
  contents <- TIO.readFile fileName

  let
    interactive = parseInteractiveScript contents
    result = runInteractive interactive
  TIO.writeFile fileName result

parseInteractiveScript :: Text -> [Interactive]
parseInteractiveScript contents =
  let
    prefixedLines = extractPrefix <$> T.lines contents
    groupedLines = List.groupBy (\l r -> fst l == fst r) prefixedLines

    toInteractive linesWithSamePrefix =
      case fst <$> linesWithSamePrefix of
        [] ->
          error "Unexpected empty list"
        Just "//":_ ->
          Just (Comment (T.intercalate "\n" (snd <$> linesWithSamePrefix)))
        Just ">>>":_ ->
          Just (Input (T.intercalate "\n" (snd <$> linesWithSamePrefix)))
        Just _:_ ->
          error "Unexpected prefix"
        Nothing:_ ->
          Nothing
  in
    Maybe.mapMaybe toInteractive groupedLines

extractPrefix :: Text -> (Maybe Text, Text)
extractPrefix line =
  case T.words line of
    "//":_ -> (Just "//", T.drop 3 line)
    ">>>":_ -> (Just ">>>", T.drop 4 line)
    _ -> (Nothing, line)

runInteractive :: [Interactive] -> Text
runInteractive interactives =
  let
    prefixLines :: Text -> Text -> Text
    prefixLines prefix text =
      T.unlines ((prefix <>) <$> T.lines text)

    go :: Interactive -> Text
    go interactive =
      case interactive of
        Comment comment ->
          prefixLines "// " comment
        Input input ->
          let
            result = Rumor.parse Rumor.newContext "" input
          in
               prefixLines ">>> " input
            <> case result of
                  Right a -> do
                    toDebugText a
                  Left err -> do
                    T.pack err
  in
    T.intercalate "\n" (go <$> interactives)

toDebugText :: ([Rumor.Node], Rumor.Context) -> Text
toDebugText (nodes, context) =
     Rumor.contextToDebugText context
  <> Rumor.nodesToDebugText nodes
  <> "\n"
