{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}
module Pipewire.Core where

import Foreign (Ptr, allocaBytes)
import Language.C.Inline qualified as C

import Data.Text (Text)
import Data.Word (Word32)
import Foreign.C (CInt, CString)

import Pipewire.CContext
import Pipewire.Internal (peekCString)

import Pipewire.Raw
import Pipewire.SPA.Utilities.CContext qualified as SPAUtils
import Pipewire.SPA.Utilities.Hooks (SpaHook (..))
import Pipewire.Protocol

C.context (C.baseCtx <> pwContext <> SPAUtils.pwContext)

C.include "<pipewire/core.h>"

newtype PwCoreInfo = PwCoreInfo (Ptr PwCoreInfoStruct)
newtype PwCoreEvents = PwCoreEvents (Ptr PwCoreEventsStruct)

type InfoHandler = PwCoreInfo -> IO ()
type InfoHandlerRaw = Ptr () -> Ptr PwCoreInfoStruct -> IO ()

type DoneHandler = PwID -> SeqID -> IO ()
type DoneHandlerRaw = Ptr () -> Word32 -> CInt -> IO ()

type ErrorHandler = Word32 -> Int -> Int -> Text -> IO ()
type ErrorHandlerRaw = Ptr () -> Word32 -> CInt -> CInt -> CString -> IO ()

-- | Create a local pw_registry_events structure
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
        cb (PwCoreEvents p)
  where
    infoWrapper _data info = infoHandler (PwCoreInfo info)
    doneWrapper _data id' seq' = doneHandler (PwID $ fromIntegral id') (SeqID $ fromIntegral seq')
    errorWrapper _data id' seq' res cMessage = do
        message <- peekCString cMessage
        errorHandler id' (fromIntegral seq') (fromIntegral res) message

    size = [C.pure| size_t {sizeof (struct pw_core_events)} |]

pw_core_add_listener :: PwCore -> SpaHook -> PwCoreEvents -> IO ()
pw_core_add_listener (PwCore core) (SpaHook hook) (PwCoreEvents pce) =
    [C.exp| void{pw_core_add_listener($(struct pw_core* core), $(struct spa_hook* hook), $(struct pw_core_events* pce), NULL)} |]

pw_core_sync :: PwCore -> PwID -> IO SeqID
pw_core_sync (PwCore core) (PwID (fromIntegral -> pwid)) =
  SeqID . fromIntegral <$>
    [C.exp| int{pw_core_sync($(struct pw_core* core), $(int pwid), 0)} |]

pw_id_core :: PwID
pw_id_core = PwID $ fromIntegral [C.pure| int{PW_ID_CORE} |]
