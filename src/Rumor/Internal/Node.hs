module Rumor.Internal.Node
  ( Node(..)
  , Label(..)
  , Speaker(..)
  , ClearType(..)

  , nodesToDebugText
  ) where

import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)
import Rumor.Internal.Expression (Expression(..), expressionToDebugText)
import Rumor.Internal.Unicode (Unicode(..), unicodeToText)
import Rumor.Internal.VariableName (VariableName(..), variableNameToText)

import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T

-- | The nodes represent the abstract syntax tree of a Rumor dialog.
data Node =
    Say (Maybe Speaker) Expression (Maybe Label)
  | Add (Maybe Speaker) Expression (Maybe Label)
  | Control Expression (NonEmpty Node) (Maybe (NonEmpty Node))
  | Action VariableName [Expression]
  | Choice Expression (Maybe Label) (NonEmpty Node)
  | Clear ClearType
  | Set VariableName Expression
  | Choose
  deriving (Eq, Show)

-- | The identifier for a node.
newtype Label = Label Unicode
  deriving (Eq, Show)

labelToText :: Label -> Text
labelToText (Label unicode) = unicodeToText unicode

-- | The identifier for a character who is saying something.
newtype Speaker = Speaker Unicode
  deriving (Eq, Show)

speakerToText :: Speaker -> Text
speakerToText (Speaker unicode) = unicodeToText unicode

data ClearType =
    ClearAll
  | ClearChoice Label
  | ClearChoices
  | ClearDialog
  deriving (Eq, Show)

nodesToDebugText :: [Node] -> Text
nodesToDebugText allNodes =
  let
    go :: Int -> [Node] -> [Text]
    go indentLevel =
      foldl (\acc node -> acc ++ ((indent indentLevel <>) <$> nodeToDebugText node)) []

    nodeToDebugText :: Node -> [Text]
    nodeToDebugText node =
      case node of
        Say speaker expression label ->
          [    maybe "" speakerToText speaker
            <> ": "
            <> expressionToDebugText expression
            <> maybe "" (\l -> " [" <> labelToText l <> "]") label
          ]
        Add speaker expression label ->
          [    maybe "" speakerToText speaker
            <> "+ "
            <> expressionToDebugText expression
            <> maybe "" (\l -> " [" <> labelToText l <> "]") label
          ]
        Control condition ifTrue mIfFalse ->
          [    "if { "
            <> expressionToDebugText condition
            <> " }"
          ] <> go 2 (NE.toList ifTrue)
            <> case mIfFalse of
                  Just ifFalse ->
                    [    "else"
                    ] <> go 2 (NE.toList ifFalse)
                  Nothing ->
                    []
        Action name expressions ->
          [    variableNameToText name
            <> "("
            <> T.intercalate ", " (expressionToDebugText <$> expressions)
            <> ")"
          ]
        Choice text label inner ->
          [    "choice"
          ,    "  > "
            <> expressionToDebugText text
            <> maybe "" (\l -> " [" <> labelToText l <> "]") label
          ] <> go 2 (NE.toList inner)
        Clear typ ->
          [    "clear"
            <> case typ of
                ClearAll -> " all"
                ClearChoice label -> " choice " <> labelToText label
                ClearChoices -> " choices"
                ClearDialog -> " dialog"
          ]
        Set name value ->
          [    variableNameToText name
            <> " = "
            <> expressionToDebugText value
          ]
        Choose ->
          [ "choose" ]

    indent :: Int -> Text
    indent indentLevel =
      T.replicate indentLevel " "

  in
    T.intercalate "\n" (go 0 allNodes)
