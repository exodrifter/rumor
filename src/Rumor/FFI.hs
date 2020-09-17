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

import qualified Rumor

import Data.Fixed (E12)
import Data.Word (Word8)
import Foreign.C.String (CString, peekCString)
import Foreign.C.Types (CULLong(..))
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Marshal.Alloc (mallocBytes, free)
import Foreign.Ptr (Ptr, castPtr, nullPtr, plusPtr)
import Foreign.StablePtr (StablePtr, deRefStablePtr, freeStablePtr, newStablePtr)
import Foreign.Storable (pokeByteOff)
import GHC.Err (error)
import GHC.Real (fromIntegral)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BSI

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
  rumor_parse :: CString -> CString -> IO ScriptPtr

rumor_parse :: CString -> CString -> IO ScriptPtr
rumor_parse c_sourceName c_text = do
  sourceName <- cstring c_sourceName
  text <- ctext c_text
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
  rumor_choose :: CString -> ContextPtr -> IO ContextPtr

rumor_choose :: CString -> ContextPtr -> IO ContextPtr
rumor_choose c_id contextPtr = do
  id <- ctext c_id
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

foreign export ccall
  rumor_current_dialog_for :: CString -> ContextPtr -> IO CString

rumor_current_dialog_for :: CString -> ContextPtr -> IO CString
rumor_current_dialog_for c_speaker contextPtr = do
  speaker <- nonEmptyText <$> ctext c_speaker
  context <- deRefStablePtr contextPtr
  case Rumor.currentDialogFor (Rumor.Character <$> speaker) context of
    Just dialog -> textToCString dialog
    Nothing -> pure $ nullPtr

--------------------------------------------------------------------------------
-- Context mutators
--------------------------------------------------------------------------------

foreign export ccall
  rumor_add_choice :: CString -> CString -> ContextPtr -> IO ContextPtr

rumor_add_choice :: CString -> CString -> ContextPtr -> IO ContextPtr
rumor_add_choice c_id c_choice contextPtr = do
  id <- ctext c_id
  choice <- ctext c_choice
  context <- deRefStablePtr contextPtr
  newStablePtr $ Rumor.addChoice (Rumor.Identifier id) choice context

foreign export ccall
  rumor_add_dialog :: CString -> CString -> ContextPtr -> IO ContextPtr

rumor_add_dialog :: CString -> CString -> ContextPtr -> IO ContextPtr
rumor_add_dialog c_character c_dialog contextPtr = do
  character <- nonEmptyText <$> ctext c_character
  dialog <- ctext c_dialog
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

cstring :: CString -> IO [Char]
cstring ptr =
  if nullPtr == ptr
  then pure []
  else peekCString ptr

-- Decodes a CString as a UTF-8 encoded bytestring.
ctext :: CString -> IO T.Text
ctext ptr =
  if nullPtr == ptr
  then pure T.empty
  else do
    TE.decodeUtf8 <$> BS.packCString ptr

-- Returns Nothing if the Text is null
nonEmptyText :: T.Text -> Maybe T.Text
nonEmptyText t =
  if T.null t
  then Nothing
  else Just t

-- Encodes a Text as a UTF-8 encoded bytestring. The allocated CString must be
-- explicity freed by rumor_free_ptr.
textToCString :: T.Text -> IO CString
textToCString t = do
  let BSI.PS fp o l = TE.encodeUtf8 t

  buf <- mallocBytes $ l + 1

  withForeignPtr fp $ \p ->
    BSI.memcpy buf (p `plusPtr` o) l
  pokeByteOff buf l (0 :: Word8)

  pure $ castPtr buf
