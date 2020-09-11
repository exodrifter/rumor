{-# LANGUAGE ForeignFunctionInterface #-}

-- This module is used to create a shared library which can be used by another
-- language which implements FFI.
module Rumor.FFI
( free

, parse
, init

, advance
, choose
, update
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
  free :: StablePtr a -> IO ()

free :: StablePtr a -> IO ()
free = freeStablePtr

--------------------------------------------------------------------------------
-- Compilation
--------------------------------------------------------------------------------

foreign export ccall
  parse :: CString -> CString -> IO ScriptPtr

parse :: CString -> CString -> IO ScriptPtr
parse c_sourceName c_text = do
  sourceName <- peekCString c_sourceName
  text <- peekCString c_text
  case Rumor.parse sourceName (T.pack text) of
    Left err -> error $ show err
    Right script -> do
      newStablePtr script

foreign export ccall
  init :: ScriptPtr -> IO ContextPtr

init :: ScriptPtr -> IO ContextPtr
init scriptPtr = do
  script <- deRefStablePtr scriptPtr
  newStablePtr $ Rumor.init script

--------------------------------------------------------------------------------
-- Context manipulation
--------------------------------------------------------------------------------

foreign export ccall
  advance :: ContextPtr -> IO ContextPtr

advance :: ContextPtr -> IO ContextPtr
advance contextPtr = do
  context <- deRefStablePtr contextPtr
  newStablePtr $ Rumor.advance context

foreign export ccall
  choose :: CString -> ContextPtr -> IO ContextPtr

choose :: CString -> ContextPtr -> IO ContextPtr
choose c_id contextPtr = do
  id <- T.pack <$> peekCString c_id
  context <- deRefStablePtr contextPtr
  newStablePtr $
    Rumor.choose (Rumor.Identifier id) context

foreign export ccall
  update :: CULLong -> ContextPtr -> IO ContextPtr

update :: CULLong -> ContextPtr -> IO ContextPtr
update delta contextPtr = do
  context <- deRefStablePtr contextPtr
  newStablePtr $ Rumor.update (fromIntegral delta) context
