module Pipewire.CoreAPI.Core where

import Language.C.Inline qualified as C

import Pipewire.CContext
import Pipewire.CoreAPI.Proxy
import Pipewire.Prelude
import Pipewire.Protocol
import Pipewire.SPA.Utilities.Hooks (SpaHook (..))
import Pipewire.Utilities.Properties (PwProperties (..))

C.context (C.baseCtx <> pwContext)

C.include "<pipewire/core.h>"

newtype Registry = Registry (Ptr PwRegistryStruct)
newtype Core = Core (Ptr PwCoreStruct)

newtype CoreInfo = CoreInfo (Ptr PwCoreInfoStruct)
newtype CoreEvents = CoreEvents (Ptr PwCoreEventsStruct)

type InfoHandler = CoreInfo -> IO ()
type InfoHandlerRaw = Ptr () -> Ptr PwCoreInfoStruct -> IO ()

type DoneHandler = PwID -> SeqID -> IO ()
type DoneHandlerRaw = Ptr () -> Word32 -> CInt -> IO ()

type ErrorHandler = PwID -> SeqID -> Int -> Text -> IO ()
type ErrorHandlerRaw = Ptr () -> Word32 -> CInt -> CInt -> CString -> IO ()

-- | Create a local pw_core_events structure
withCoreEvents :: InfoHandler -> DoneHandler -> ErrorHandler -> (CoreEvents -> IO b) -> IO b
withCoreEvents infoHandler doneHandler errorHandler cb = allocaBytes
    (fromIntegral size)
    \p -> do
        infoP <- $(C.mkFunPtr [t|InfoHandlerRaw|]) infoWrapper
        doneP <- $(C.mkFunPtr [t|DoneHandlerRaw|]) doneWrapper
        errorP <- $(C.mkFunPtr [t|ErrorHandlerRaw|]) errorWrapper
        [C.block| void{
                struct pw_core_events* pre = $(struct pw_core_events* p);
                pre->version = PW_VERSION_CORE_EVENTS;
                pre->info = $(void (*infoP)(void*, const struct pw_core_info *));
                pre->done = $(void (*doneP)(void*, uint32_t id, int seq));
                pre->error = $(void (*errorP)(void*, uint32_t id, int seq, int res, const char* message));
        }|]
        cb (CoreEvents p) `finally` do
            freeHaskellFunPtr infoP
            freeHaskellFunPtr doneP
            freeHaskellFunPtr errorP
  where
    infoWrapper _data info = infoHandler (CoreInfo info)
    doneWrapper _data id' seq' = doneHandler (PwID $ fromIntegral id') (SeqID $ fromIntegral seq')
    errorWrapper _data id' seq' res cMessage = do
        message <- peekCString cMessage
        errorHandler (PwID $ fromIntegral id') (SeqID $ fromIntegral seq') (fromIntegral res) message

    size = [C.pure| size_t {sizeof (struct pw_core_events)} |]

pw_core_add_listener :: Core -> SpaHook -> CoreEvents -> IO ()
pw_core_add_listener (Core core) (SpaHook hook) (CoreEvents pce) =
    [C.exp| void{pw_core_add_listener($(struct pw_core* core), $(struct spa_hook* hook), $(struct pw_core_events* pce), NULL)} |]

pw_core_sync :: Core -> PwID -> IO SeqID
pw_core_sync (Core core) (PwID (fromIntegral -> pwid)) =
    SeqID . fromIntegral
        <$> [C.exp| int{pw_core_sync($(struct pw_core* core), $(int pwid), 0)} |]

pw_id_core :: PwID
pw_id_core = PwID $ fromIntegral [C.pure| int{PW_ID_CORE} |]

pw_core_disconnect :: Core -> IO ()
pw_core_disconnect (Core core) =
    [C.exp| void{pw_core_disconnect($(struct pw_core* core))} |]

pw_core_get_registry :: Core -> IO Registry
pw_core_get_registry (Core core) =
    Registry
        <$> dieOnNull
            "pw_core_get_registry"
            [C.exp| struct pw_registry*{pw_core_get_registry($(struct pw_core* core), PW_VERSION_REGISTRY, 0)} |]

pw_core_create_object :: Core -> Text -> Text -> PwVersion -> PwProperties -> IO PwProxy
pw_core_create_object (Core core) factoryName typeName (PwVersion (fromIntegral -> version)) (PwProperties props) =
    withCString factoryName \cFactoryName ->
        withCString typeName \cType ->
            PwProxy
                <$> dieOnNull
                    "pw_core_create_object"
                    [C.exp| struct pw_proxy*{pw_core_create_object(
                          $(struct pw_core* core),
                          $(const char* cFactoryName),
                          $(const char* cType),
                          $(int version),
                          &$(const struct pw_properties* props)->dict,
                          0
                    )} |]
