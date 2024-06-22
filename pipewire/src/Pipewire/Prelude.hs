-- | Helper functions and re-export
module Pipewire.Prelude (
    peekCString,
    dieOnNull,
    dieOnErr,
    maybeOnNull,

    -- * Base
    module Data.Word,
    module Control.Exception,
    module Control.Monad,

    -- * Foreign
    module Foreign,
    module Foreign.C.Types,
    module Foreign.C.String,
    fromBool,

    -- * text
    Text,
    withCString,

    -- * vector
    Vector,
    cfloatVector,
) where

import Control.Exception (finally)
import Control.Monad (unless, void, when)
import Data.ByteString.Unsafe (unsafePackCString)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import Data.Text.Foreign (withCString)
import Data.Vector.Storable (Vector)
import Data.Vector.Storable qualified as SV
import Data.Word
import Foreign (FunPtr, Ptr, alloca, allocaBytes, freeHaskellFunPtr, nullPtr)
import Foreign.C.String (CString)
import Foreign.C.Types (CFloat, CInt)
import Foreign.C.Types qualified (CFloat (..))
import Foreign.Marshal.Utils (fromBool)

peekCString :: CString -> IO Text
peekCString cs = do
    bs <- unsafePackCString cs
    return $! decodeUtf8 bs

dieOnErr :: String -> IO CInt -> IO ()
dieOnErr src act = do
    v <- act
    when (v < 0) do
        ioError $ userError $ src <> " returned " <> show v

dieOnNull :: String -> IO (Ptr a) -> IO (Ptr a)
dieOnNull src act = do
    ptr <- act
    if ptr == nullPtr
        then ioError $ userError $ src <> " returned NULL"
        else pure ptr

maybeOnNull :: (Ptr a -> b) -> Ptr a -> Maybe b
maybeOnNull mk ptr
    | ptr == nullPtr = Nothing
    | otherwise = Just (mk ptr)

cfloatVector :: SV.Vector Float -> SV.Vector CFloat
cfloatVector = SV.unsafeCoerceVector
