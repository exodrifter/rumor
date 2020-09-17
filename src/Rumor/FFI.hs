{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- This module is used to create a shared library which can be used by another
-- language which implements FFI.
--
-- Note that all functions are prefixed with `rumor_`. This is to avoid
-- accidentally creating a name conflict with an already-existing function. If
-- we accidentally define a function with the same name as one that the Haskell
-- run-time system also uses, then the run-time system may invoke our function
-- instead of the one it expects which can result in a segmentation fault crash
-- at runtime.
module Rumor.FFI
( rumor_free_ptr
, rumor_free_stable_ptr

, rumor_parse
, rumor_init

, rumor_advance
, rumor_choose
, rumor_update

, rumor_current_dialog_for

, rumor_add_choice
, rumor_add_dialog
, rumor_clear_all
, rumor_clear_choices
, rumor_clear_dialog
) where

import Rumor.FFI.Types
import qualified Rumor

import Data.Fixed (E12)
import Foreign.C.Types (CULLong(..))
import Foreign.CStorable (CStorable(..))
import Foreign.Marshal.Alloc (free)
import Foreign.Marshal.Array (newArray0, peekArray0)
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.StablePtr (StablePtr, deRefStablePtr, freeStablePtr, newStablePtr)
import Foreign.Storable (Storable(..))
import GHC.Err (error)
import GHC.Generics (Generic)
import GHC.Real (fromIntegral)
import qualified Data.Text as T
import qualified Data.Map.Strict as Map

type ScriptPtr = StablePtr (Rumor.Script E12)
type ContextPtr = StablePtr (Rumor.Context E12)

--------------------------------------------------------------------------------
-- Pointer helpers
--------------------------------------------------------------------------------

foreign export ccall
  rumor_free_ptr :: Ptr a -> IO ()

rumor_free_ptr :: Ptr a -> IO ()
rumor_free_ptr ptr =
  if nullPtr == ptr
  then pure ()
  else free ptr

foreign export ccall
  rumor_free_stable_ptr :: StablePtr a -> IO ()

rumor_free_stable_ptr :: StablePtr a -> IO ()
rumor_free_stable_ptr = freeStablePtr

--------------------------------------------------------------------------------
-- Compilation
--------------------------------------------------------------------------------

foreign export ccall
  rumor_parse :: CString -> CUtf8 -> IO ScriptPtr

rumor_parse :: CString -> CUtf8 -> IO ScriptPtr
rumor_parse c_sourceName c_text = do
  sourceName <- readCString c_sourceName
  text <- readCUtf8 c_text
  case Rumor.parse sourceName text of
    Left err -> error $ show err
    Right script -> do
      newStablePtr script

foreign export ccall
  rumor_init :: ScriptPtr -> IO ContextPtr

rumor_init :: ScriptPtr -> IO ContextPtr
rumor_init scriptPtr = do
  script <- deRefStablePtr scriptPtr
  newStablePtr $ Rumor.init script

--------------------------------------------------------------------------------
-- Context manipulation
--------------------------------------------------------------------------------

foreign export ccall
  rumor_advance :: ContextPtr -> IO ContextPtr

rumor_advance :: ContextPtr -> IO ContextPtr
rumor_advance contextPtr = do
  context <- deRefStablePtr contextPtr
  newStablePtr $ Rumor.advance context

foreign export ccall
  rumor_choose :: CUtf8 -> ContextPtr -> IO ContextPtr

rumor_choose :: CUtf8 -> ContextPtr -> IO ContextPtr
rumor_choose c_id contextPtr = do
  id <- readCUtf8 c_id
  context <- deRefStablePtr contextPtr
  newStablePtr $
    Rumor.choose (Rumor.Identifier id) context

foreign export ccall
  rumor_update :: CULLong -> ContextPtr -> IO ContextPtr

rumor_update :: CULLong -> ContextPtr -> IO ContextPtr
rumor_update delta contextPtr = do
  context <- deRefStablePtr contextPtr
  newStablePtr $ Rumor.update (fromIntegral delta) context

--------------------------------------------------------------------------------
-- Context getters
--------------------------------------------------------------------------------

data Choice = Choice CUtf8 CUtf8
  deriving stock (Eq, Generic, Show)
  deriving anyclass (CStorable)

instance Storable Choice where
 peek      = cPeek
 poke      = cPoke
 alignment = cAlignment
 sizeOf    = cSizeOf

nullChoice :: Choice
nullChoice = Choice nullCUtf8 nullCUtf8

newChoice :: (Rumor.Identifier, T.Text) -> IO Choice
newChoice (id, t) =
  Choice
    <$> newCUtf8 (Rumor.unIdentifier id)
    <*> newCUtf8 t

freeChoice :: Choice -> IO ()
freeChoice (Choice id t) = do
  rumor_free_ptr $ unCUtf8 id
  rumor_free_ptr $ unCUtf8 t

foreign export ccall
  rumor_free_current_choices :: Ptr Choice -> IO ()

rumor_free_current_choices :: Ptr Choice -> IO ()
rumor_free_current_choices choicePtr = do
  choices <- peekArray0 nullChoice choicePtr
  _ <- sequence $ freeChoice <$> choices
  free choicePtr

foreign export ccall
  rumor_current_choices :: ContextPtr -> IO (Ptr Choice)

rumor_current_choices :: ContextPtr -> IO (Ptr Choice)
rumor_current_choices contextPtr = do
  context <- deRefStablePtr contextPtr
  let c = Map.toList $ Rumor.currentChoices context
  choices <- sequence $ newChoice <$> c
  newArray0 nullChoice choices

foreign export ccall
  rumor_current_dialog_for :: CUtf8 -> ContextPtr -> IO CUtf8

rumor_current_dialog_for :: CUtf8 -> ContextPtr -> IO CUtf8
rumor_current_dialog_for c_speaker contextPtr = do
  speaker <- nonEmptyText <$> readCUtf8 c_speaker
  context <- deRefStablePtr contextPtr
  case Rumor.currentDialogFor (Rumor.Character <$> speaker) context of
    Just dialog -> newCUtf8 dialog
    Nothing -> pure nullCUtf8

--------------------------------------------------------------------------------
-- Context mutators
--------------------------------------------------------------------------------

foreign export ccall
  rumor_add_choice :: CUtf8 -> CUtf8 -> ContextPtr -> IO ContextPtr

rumor_add_choice :: CUtf8 -> CUtf8 -> ContextPtr -> IO ContextPtr
rumor_add_choice c_id c_choice contextPtr = do
  id <- readCUtf8 c_id
  choice <- readCUtf8 c_choice
  context <- deRefStablePtr contextPtr
  newStablePtr $ Rumor.addChoice (Rumor.Identifier id) choice context

foreign export ccall
  rumor_add_dialog :: CUtf8 -> CUtf8 -> ContextPtr -> IO ContextPtr

rumor_add_dialog :: CUtf8 -> CUtf8 -> ContextPtr -> IO ContextPtr
rumor_add_dialog c_character c_dialog contextPtr = do
  character <- nonEmptyText <$> readCUtf8 c_character
  dialog <- readCUtf8 c_dialog
  context <- deRefStablePtr contextPtr
  newStablePtr $ Rumor.addDialog (Rumor.Character <$> character) dialog context

foreign export ccall
  rumor_clear_all :: ContextPtr -> IO ContextPtr

rumor_clear_all :: ContextPtr -> IO ContextPtr
rumor_clear_all contextPtr = do
  context <- deRefStablePtr contextPtr
  newStablePtr $ Rumor.clearAll context

foreign export ccall
  rumor_clear_choices :: ContextPtr -> IO ContextPtr

rumor_clear_choices :: ContextPtr -> IO ContextPtr
rumor_clear_choices contextPtr = do
  context <- deRefStablePtr contextPtr
  newStablePtr $ Rumor.clearChoices context

foreign export ccall
  rumor_clear_dialog :: ContextPtr -> IO ContextPtr

rumor_clear_dialog :: ContextPtr -> IO ContextPtr
rumor_clear_dialog contextPtr = do
  context <- deRefStablePtr contextPtr
  newStablePtr $ Rumor.clearDialog context

--------------------------------------------------------------------------------
-- Utility functions
--------------------------------------------------------------------------------

-- Returns Nothing if the Text is null
nonEmptyText :: T.Text -> Maybe T.Text
nonEmptyText t =
  if T.null t
  then Nothing
  else Just t
