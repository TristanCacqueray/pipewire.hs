module Pipewire where

import Data.Text (Text)
import Data.Word (Word32)

import Control.Exception (bracket, bracket_)
import Pipewire.Internal
import Pipewire.Raw qualified as Raw
import Pipewire.Structs

newtype PwID = PwID Word32
    deriving newtype (Show)

withPipewire :: IO a -> IO a
withPipewire = bracket_ Raw.pw_init Raw.pw_deinit

withMainLoop :: (PwMainLoop -> IO a) -> IO a
withMainLoop = bracket Raw.pw_main_loop_new Raw.pw_main_loop_destroy

withContext :: PwLoop -> (PwContext -> IO a) -> IO a
withContext loop = bracket (Raw.pw_context_new loop) Raw.pw_context_destroy

withCore :: PwContext -> (PwCore -> IO a) -> IO a
withCore context = bracket (Raw.pw_context_connect context) Raw.pw_core_disconnect

getHeadersVersion :: IO Text
getHeadersVersion = Raw.pw_get_headers_version >>= peekCString

getLibraryVersion :: IO Text
getLibraryVersion = Raw.pw_get_library_version >>= peekCString

withRegistryEvents :: (PwID -> Text -> SpaDict -> IO ()) -> (PwRegistryEvents -> IO a) -> IO a
withRegistryEvents handler = Raw.pw_with_registry_event wrapper
  where
    wrapper cuint cstr props = do
        txt <- peekCString cstr
        handler (PwID cuint) txt props
