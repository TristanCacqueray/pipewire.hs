-- | Helper functions and re-export
module Pipewire.Internal (
    peekCString,

    -- * Base
    Word32,

    -- * Foreign
    CInt,
    CString,
    Ptr,
    nullPtr,

    -- * text
    Text,
    withCString,
) where

import Data.ByteString.Unsafe (unsafePackCString)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import Data.Text.Foreign (withCString)
import Data.Word (Word32)
import Foreign (Ptr, nullPtr)
import Foreign.C.String (CString)
import Foreign.C.Types (CInt)

peekCString :: CString -> IO Text
peekCString cs = do
    bs <- unsafePackCString cs
    return $! decodeUtf8 bs
