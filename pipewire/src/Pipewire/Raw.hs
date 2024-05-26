{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}

-- | The RAW bindings to the libpipewire, using 'Foreign.C.Types' and 'Pipewire.Structs'
module Pipewire.Raw where

import Foreign.C.String (CString)
import Language.C.Inline qualified as C

import Data.Word
import Foreign (Ptr, allocaBytes)
import Foreign.C.Types
import Pipewire.Context
import Pipewire.Structs

C.context (C.baseCtx <> C.bsCtx <> pwContext <> C.funCtx)
C.include "<pipewire/pipewire.h>"

pw_init :: IO ()
pw_init = [C.exp| void{pw_init(NULL, NULL)} |]

pw_get_headers_version :: IO CString
pw_get_headers_version = [C.exp| const char*{pw_get_headers_version()} |]

pw_get_library_version :: IO CString
pw_get_library_version = [C.exp| const char*{pw_get_library_version()} |]

pw_main_loop_new :: IO PwMainLoop
pw_main_loop_new = PwMainLoop <$> [C.exp| struct pw_main_loop*{pw_main_loop_new(NULL)} |]

pw_main_loop_get_loop :: PwMainLoop -> IO PwLoop
pw_main_loop_get_loop (PwMainLoop mainLoop) =
    PwLoop <$> [C.exp| struct pw_loop*{pw_main_loop_get_loop($(struct pw_main_loop* mainLoop))} |]

pw_main_loop_destroy :: PwMainLoop -> IO ()
pw_main_loop_destroy (PwMainLoop mainLoop) =
    [C.exp| void{pw_main_loop_destroy($(struct pw_main_loop* mainLoop))} |]

pw_main_loop_run :: PwMainLoop -> IO CInt
pw_main_loop_run (PwMainLoop mainLoop) =
    [C.exp| int{pw_main_loop_run($(struct pw_main_loop* mainLoop))} |]

pw_main_loop_quit :: PwMainLoop -> IO CInt
pw_main_loop_quit (PwMainLoop mainLoop) =
    [C.exp| int{pw_main_loop_quit($(struct pw_main_loop* mainLoop))} |]

pw_context_new :: PwLoop -> IO PwContext
pw_context_new (PwLoop loop) =
    PwContext <$> [C.exp| struct pw_context*{pw_context_new($(struct pw_loop* loop), NULL, 0)} |]

pw_context_connect :: PwContext -> IO PwCore
pw_context_connect (PwContext ctx) =
    PwCore <$> [C.exp| struct pw_core*{pw_context_connect($(struct pw_context* ctx), NULL, 0)} |]

pw_core_disconnect :: PwCore -> IO ()
pw_core_disconnect (PwCore core) =
    [C.exp| void{pw_core_disconnect($(struct pw_core* core))} |]

pw_context_destroy :: PwContext -> IO ()
pw_context_destroy (PwContext ctx) =
    [C.exp| void{pw_context_destroy($(struct pw_context* ctx))} |]

pw_core_get_registry :: PwCore -> IO PwRegistry
pw_core_get_registry (PwCore core) =
    PwRegistry <$> [C.exp| struct pw_registry*{pw_core_get_registry($(struct pw_core* core), PW_VERSION_REGISTRY, 0)} |]

-- | Create a local spa_hook structure
pw_with_spa_hook :: (SpaHook -> IO a) -> IO a
pw_with_spa_hook cb = allocaBytes
    (fromIntegral size)
    \p -> do
        -- Do we need to memset after allocaBytes ?
        [C.exp| void{spa_memzero($(struct spa_hook* p), $(size_t size))} |]
        cb (SpaHook p)
  where
    size = [C.pure| size_t {sizeof (struct spa_hook)} |]

type GlobalHandler = Ptr () -> Word32 -> Word32 -> CString -> Word32 -> Ptr SpaDictStruct -> IO ()

-- | Create a local pw_registry_events structure
pw_with_registry_event :: (Word32 -> CString -> IO ()) -> (PwRegistryEvents -> IO b) -> IO b
pw_with_registry_event handler cb = allocaBytes
    (fromIntegral size)
    \p -> do
        handlerP <- $(C.mkFunPtr [t|GlobalHandler|]) wrapper
        [C.block| void{
                struct pw_registry_events* pre = $(struct pw_registry_events* p);
                pre->version = PW_VERSION_REGISTRY_EVENTS;
                pre->global = $(void (*handlerP)(void*, uint32_t, uint32_t, const char*, uint32_t version, const struct spa_dict * props));
                pre->global_remove = NULL;
        }|]
        cb (PwRegistryEvents p)
  where
    wrapper _data pwid _version name _ _ = handler pwid name

    size = [C.pure| size_t {sizeof (struct pw_registry_events)} |]

pw_registry_add_listener :: PwRegistry -> SpaHook -> PwRegistryEvents -> IO ()
pw_registry_add_listener (PwRegistry registry) (SpaHook hook) (PwRegistryEvents pre) =
    [C.exp| void{pw_registry_add_listener($(struct pw_registry* registry), $(struct spa_hook* hook), $(struct pw_registry_events* pre), NULL)} |]
