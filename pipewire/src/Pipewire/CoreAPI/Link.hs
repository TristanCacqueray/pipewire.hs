module Pipewire.CoreAPI.Link where

import Control.Exception (finally)
import Foreign (allocaBytes, freeHaskellFunPtr)
import Language.C.Inline qualified as C

import Pipewire.CoreAPI.CContext
import Pipewire.CoreAPI.Core
import Pipewire.CoreAPI.Proxy
import Pipewire.Enum
import Pipewire.Internal
import Pipewire.Protocol (PwID (..))
import Pipewire.SPA.CContext qualified as SPAUtils
import Pipewire.SPA.Utilities.Hooks (SpaHook (..), with_spa_hook)
import Pipewire.Utilities.Properties (PwProperties)

C.context (C.baseCtx <> pwContext <> SPAUtils.pwContext)

C.include "<pipewire/link.h>"

newtype PwLink = PwLink {getProxy :: PwProxy}

pw_link_create :: PwCore -> PwProperties -> IO PwLink
pw_link_create core props =
    -- TODO: check for nullPtr
    PwLink <$> pw_core_create_object core "link-factory" "PipeWire:Interface:Link" mPW_VERSION_LINK props

newtype NodeID = NodeeID PwID
newtype PortID = PortID PwID

type LinkState = Either Text PwLinkState
type PwLinkEventInfoHandler = PwID -> LinkState -> IO ()

with_pw_link_events :: PwLink -> PwLinkEventInfoHandler -> IO a -> IO a
with_pw_link_events (PwLink (PwProxy pwProxy)) infoHandler cb = with_spa_hook \(SpaHook spaHook) -> allocaBytes
    (fromIntegral [C.pure| size_t {sizeof (struct pw_link_events)} |])
    \p -> do
        infoP <- $(C.mkFunPtr [t|Ptr () -> Ptr PwLinkInfoStruct -> IO ()|]) wrapper
        [C.block| void{
                struct pw_link_events* ple = $(struct pw_link_events* p);
                ple->version = PW_VERSION_LINK_EVENTS;
                ple->info = $(void (*infoP)(void*, const struct pw_link_info*));
                pw_proxy_add_object_listener(
                  $(struct pw_proxy* pwProxy),
                  $(struct spa_hook* spaHook),
                  ple,
                  NULL);
        }|]
        cb
            `finally` do
                freeHaskellFunPtr infoP
  where
    wrapper _data ptr = do
        pwid <- PwID . fromIntegral <$> [C.exp| int{$(struct pw_link_info* ptr)->id}|]
        (state :: PwLinkState) <- PwLinkState <$> [C.exp| int{$(struct pw_link_info* ptr)->state} |]
        case state of
            PW_LINK_STATE_ERROR -> do
                (errC :: CString) <- [C.exp| const char*{$(struct pw_link_info* ptr)->error} |]
                err <- peekCString errC
                infoHandler pwid (Left err)
            _ -> infoHandler pwid (Right state)
