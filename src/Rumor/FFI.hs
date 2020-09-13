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
( rumor_free

, rumor_parse
, rumor_init

, rumor_advance
, rumor_choose
, rumor_update
) where

import qualified Rumor

import Data.Fixed (E12)
import Foreign.C.String (CString, peekCString)
import Foreign.C.Types (CULLong(..))
import Foreign.StablePtr (StablePtr, deRefStablePtr, freeStablePtr, newStablePtr)
import GHC.Err (error)
import GHC.Real (fromIntegral)
import qualified Data.Text as T

type ScriptPtr = StablePtr (Rumor.Script E12)
type ContextPtr = StablePtr (Rumor.Context E12)

--------------------------------------------------------------------------------
-- Pointer helpers
--------------------------------------------------------------------------------

foreign export ccall
  rumor_free :: StablePtr a -> IO ()

rumor_free :: StablePtr a -> IO ()
rumor_free = freeStablePtr

--------------------------------------------------------------------------------
-- Compilation
--------------------------------------------------------------------------------

foreign export ccall
  rumor_parse :: CString -> CString -> IO ScriptPtr

rumor_parse :: CString -> CString -> IO ScriptPtr
rumor_parse c_sourceName c_text = do
  sourceName <- peekCString c_sourceName
  text <- peekCString c_text
  case Rumor.parse sourceName (T.pack text) of
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
  id <- T.pack <$> peekCString c_id
  context <- deRefStablePtr contextPtr
  newStablePtr $
    Rumor.choose (Rumor.Identifier id) context

foreign export ccall
  rumor_update :: CULLong -> ContextPtr -> IO ContextPtr

rumor_update :: CULLong -> ContextPtr -> IO ContextPtr
rumor_update delta contextPtr = do
  context <- deRefStablePtr contextPtr
  newStablePtr $ Rumor.update (fromIntegral delta) context
