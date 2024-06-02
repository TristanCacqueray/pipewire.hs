module Pipewire.CoreAPI.Core where

import Language.C.Inline qualified as C

import Pipewire.CoreAPI.CContext
import Pipewire.CoreAPI.Proxy
import Pipewire.Prelude
import Pipewire.Protocol
import Pipewire.SPA.CContext qualified as SPAUtils
import Pipewire.SPA.Utilities.Hooks (SpaHook (..))
import Pipewire.Utilities.CContext qualified as Utils
import Pipewire.Utilities.Properties (PwProperties (..))

C.context (C.baseCtx <> pwContext <> SPAUtils.pwContext <> Utils.pwContext)

C.include "<pipewire/core.h>"

newtype PwRegistry = PwRegistry (Ptr PwRegistryStruct)
newtype PwCore = PwCore (Ptr PwCoreStruct)

newtype PwCoreInfo = PwCoreInfo (Ptr PwCoreInfoStruct)
newtype PwCoreEvents = PwCoreEvents (Ptr PwCoreEventsStruct)

type InfoHandler = PwCoreInfo -> IO ()
type InfoHandlerRaw = Ptr () -> Ptr PwCoreInfoStruct -> IO ()

type DoneHandler = PwID -> SeqID -> IO ()
type DoneHandlerRaw = Ptr () -> Word32 -> CInt -> IO ()

type ErrorHandler = PwID -> SeqID -> Int -> Text -> IO ()
type ErrorHandlerRaw = Ptr () -> Word32 -> CInt -> CInt -> CString -> IO ()

-- | Create a local pw_core_events structure
with_pw_core_events :: InfoHandler -> DoneHandler -> ErrorHandler -> (PwCoreEvents -> IO b) -> IO b
with_pw_core_events infoHandler doneHandler errorHandler cb = allocaBytes
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
        cb (PwCoreEvents p) `finally` do
            freeHaskellFunPtr infoP
            freeHaskellFunPtr doneP
            freeHaskellFunPtr errorP
  where
    infoWrapper _data info = infoHandler (PwCoreInfo info)
    doneWrapper _data id' seq' = doneHandler (PwID $ fromIntegral id') (SeqID $ fromIntegral seq')
    errorWrapper _data id' seq' res cMessage = do
        message <- peekCString cMessage
        errorHandler (PwID $ fromIntegral id') (SeqID $ fromIntegral seq') (fromIntegral res) message

    size = [C.pure| size_t {sizeof (struct pw_core_events)} |]

pw_core_add_listener :: PwCore -> SpaHook -> PwCoreEvents -> IO ()
pw_core_add_listener (PwCore core) (SpaHook hook) (PwCoreEvents pce) =
    [C.exp| void{pw_core_add_listener($(struct pw_core* core), $(struct spa_hook* hook), $(struct pw_core_events* pce), NULL)} |]

pw_core_sync :: PwCore -> PwID -> IO SeqID
pw_core_sync (PwCore core) (PwID (fromIntegral -> pwid)) =
    SeqID . fromIntegral
        <$> [C.exp| int{pw_core_sync($(struct pw_core* core), $(int pwid), 0)} |]

pw_id_core :: PwID
pw_id_core = PwID $ fromIntegral [C.pure| int{PW_ID_CORE} |]

pw_core_disconnect :: PwCore -> IO ()
pw_core_disconnect (PwCore core) =
    [C.block| void{
            struct pw_core* core = $(struct pw_core* core);
            if (core) pw_core_disconnect(core);
    } |]

pw_core_get_registry :: PwCore -> IO PwRegistry
pw_core_get_registry (PwCore core) =
    PwRegistry <$> [C.exp| struct pw_registry*{pw_core_get_registry($(struct pw_core* core), PW_VERSION_REGISTRY, 0)} |]

pw_core_create_object :: PwCore -> Text -> Text -> PwVersion -> PwProperties -> IO PwProxy
pw_core_create_object (PwCore core) factoryName typeName (PwVersion (fromIntegral -> version)) (PwProperties props) =
    withCString factoryName \cFactoryName ->
        withCString typeName \cType ->
            PwProxy
                <$> [C.exp| struct pw_proxy*{pw_core_create_object(
                          $(struct pw_core* core),
                          $(const char* cFactoryName),
                          $(const char* cType),
                          $(int version),
                          &$(const struct pw_properties* props)->dict,
                          0
                    )} |]
