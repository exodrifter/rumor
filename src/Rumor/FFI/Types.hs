module Rumor.FFI.Types
( CUtf8(..)
, newCUtf8
, nullCUtf8
, readCUtf8

, CString
, readCString
) where

import Data.Word (Word8)
import Foreign.Marshal.Alloc (mallocBytes)
import Foreign.C.String (CString, peekCString)
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Ptr (castPtr, nullPtr, plusPtr)
import Foreign.Storable (Storable, pokeByteOff)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BSI
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

-- Represents a UTF-8 encoded string in C.
newtype CUtf8 = CUtf8 CString
  deriving newtype (Eq, Show, Storable)

-- Marshal a Haskell Text into a NUL terminated UTF-8 encoded C string.
-- * The Haskell string may not contain any NUL characters.
-- * New storage is allocated for the C string and must be explicitly freed
--   using `free` or `finalizerFree`.
newCUtf8 :: T.Text -> IO CUtf8
newCUtf8 t = do
  let BSI.PS fp o l = TE.encodeUtf8 t

  buf <- mallocBytes $ l + 1

  withForeignPtr fp $ \p ->
    BSI.memcpy buf (p `plusPtr` o) l
  pokeByteOff buf l (0 :: Word8)

  pure $ CUtf8 (castPtr buf)

-- A CUtf8 Ptr that is not associated with a valid memory location.
nullCUtf8 :: CUtf8
nullCUtf8 = CUtf8 nullPtr

-- Marshal a NUL terminated UTF-8 encoded C string into a Haskell Text. If the
-- pointer is null, returns an empty text.
readCUtf8 :: CUtf8 -> IO T.Text
readCUtf8 (CUtf8 c_str) =
  if nullPtr == c_str
  then pure T.empty
  else do
    TE.decodeUtf8 <$> BS.packCString c_str

-- Marshal a NUL terminated C string into a Haskell string. If the pointer is
-- null, returns an empty text.
readCString :: CString -> IO [Char]
readCString ptr =
  if nullPtr == ptr
  then pure []
  else peekCString ptr
