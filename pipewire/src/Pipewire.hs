module Pipewire where

import Data.Text (Text)
import Data.Word (Word32)

import Pipewire.Internal
import Pipewire.Raw qualified as Raw
import Pipewire.Structs

newtype PwID = PwID Word32
    deriving newtype (Show)

pwInit :: IO ()
pwInit = Raw.pw_init

pwGetHeadersVersion :: IO Text
pwGetHeadersVersion = Raw.pw_get_headers_version >>= peekCString

pwGetLibraryVersion :: IO Text
pwGetLibraryVersion = Raw.pw_get_library_version >>= peekCString

pwWithRegistryEvents :: (PwID -> Text -> IO ()) -> (PwRegistryEvents -> IO a) -> IO a
pwWithRegistryEvents handler = Raw.pw_with_registry_event wrapper
  where
    wrapper cuint cstr = do
        txt <- peekCString cstr
        handler (PwID cuint) txt
