module Pipewire.CoreAPI.Registry where

import Language.C.Inline qualified as C

import Pipewire.CContext
import Pipewire.CoreAPI.Core (PwRegistry (..))
import Pipewire.Prelude
import Pipewire.Protocol
import Pipewire.SPA.Utilities.Dictionary (SpaDict (..))
import Pipewire.SPA.Utilities.Hooks (SpaHook (..))

C.context (C.baseCtx <> pwContext)

C.include "<pipewire/core.h>"

newtype PwRegistryEvents = PwRegistryEvents (Ptr PwRegistryEventsStruct)

type GlobalHandler = PwID -> Text -> PwVersion -> SpaDict -> IO ()
type GlobalHandlerRaw = Ptr () -> Word32 -> Word32 -> CString -> Word32 -> Ptr SpaDictStruct -> IO ()

type GlobalRemoveHandler = PwID -> IO ()
type GlobalRemoveHandlerRaw = Ptr () -> Word32 -> IO ()

-- | Create a local pw_registry_events structure
withRegistryEvents :: GlobalHandler -> GlobalRemoveHandler -> (PwRegistryEvents -> IO b) -> IO b
withRegistryEvents globalHandler globalRemoveHandler cb = allocaBytes
    (fromIntegral size)
    \p -> do
        globalP <- $(C.mkFunPtr [t|GlobalHandlerRaw|]) globalWrapper
        globalRemoveP <- $(C.mkFunPtr [t|GlobalRemoveHandlerRaw|]) globalRemoveWrapper
        [C.block| void{
                struct pw_registry_events* pre = $(struct pw_registry_events* p);
                pre->version = PW_VERSION_REGISTRY_EVENTS;
                pre->global = $(void (*globalP)(void*, uint32_t, uint32_t, const char*, uint32_t version, const struct spa_dict * props));
                pre->global_remove = $(void (*globalRemoveP)(void*, uint32_t));
        }|]
        cb (PwRegistryEvents p) `finally` do
            freeHaskellFunPtr globalP
            freeHaskellFunPtr globalRemoveP
  where
    globalWrapper _data pwid _permission cName version props = do
        name <- peekCString cName
        globalHandler (PwID $ fromIntegral pwid) name (PwVersion $ fromIntegral version) (SpaDict props)
    globalRemoveWrapper _data id' = globalRemoveHandler (PwID $ fromIntegral id')

    size = [C.pure| size_t {sizeof (struct pw_registry_events)} |]

pw_registry_add_listener :: PwRegistry -> SpaHook -> PwRegistryEvents -> IO ()
pw_registry_add_listener (PwRegistry registry) (SpaHook hook) (PwRegistryEvents pre) =
    [C.exp| void{pw_registry_add_listener($(struct pw_registry* registry), $(struct spa_hook* hook), $(struct pw_registry_events* pre), NULL)} |]

pw_registry_destroy :: PwRegistry -> PwID -> IO ()
pw_registry_destroy (PwRegistry registry) (PwID (fromIntegral -> pwid)) = do
    [C.exp| void{pw_registry_destroy($(struct pw_registry* registry), $(int pwid))}|]
