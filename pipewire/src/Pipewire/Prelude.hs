-- | Helper functions and re-export
module Pipewire.Prelude (
    peekCString,
    dieOnNull,
    maybeOnNull,

    -- * Base
    module Data.Word,
    module Control.Exception,
    module Control.Monad,

    -- * Foreign
    module Foreign,
    module Foreign.C.Types,
    module Foreign.C.String,

    -- * text
    Text,
    withCString,
) where

import Control.Exception (finally)
import Control.Monad (unless, void, when)
import Data.ByteString.Unsafe (unsafePackCString)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import Data.Text.Foreign (withCString)
import Data.Word
import Foreign (FunPtr, Ptr, alloca, allocaBytes, freeHaskellFunPtr, nullPtr)
import Foreign.C.String (CString)
import Foreign.C.Types (CInt)

peekCString :: CString -> IO Text
peekCString cs = do
    bs <- unsafePackCString cs
    return $! decodeUtf8 bs

dieOnNull :: String -> Ptr a -> Ptr a
dieOnNull src ptr
    | ptr == nullPtr = error $ src <> " returned NULL"
    | otherwise = ptr

maybeOnNull :: (Ptr a -> b) -> Ptr a -> Maybe b
maybeOnNull mk ptr
    | ptr == nullPtr = Nothing
    | otherwise = Just (mk ptr)
