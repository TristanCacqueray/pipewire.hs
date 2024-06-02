module Pipewire.CoreAPI.Link where

import Foreign (castPtr)
import Language.C.Inline qualified as C

import Pipewire.CoreAPI.CContext
import Pipewire.CoreAPI.Core
import Pipewire.CoreAPI.Proxy
import Pipewire.Enum
import Pipewire.Prelude
import Pipewire.Protocol (PwID (..))
import Pipewire.SPA.CContext qualified as SPAUtils
import Pipewire.SPA.Utilities.Hooks (with_spa_hook)
import Pipewire.Utilities.Properties (PwProperties, pw_properties_new, pw_properties_set_id, pw_properties_set_linger)

C.context (C.baseCtx <> pwContext <> SPAUtils.pwContext)

C.include "<pipewire/link.h>"

newtype PwLink = PwLink {getProxy :: PwProxy}

data LinkProperties = LinkProperties
    { portOutput :: PwID
    , portInput :: PwID
    , linger :: Bool
    -- ^ Set to True to keep the link after the program quit
    }

withLink :: PwCore -> LinkProperties -> (PwLink -> IO a) -> IO a
withLink core linkProperties cb = do
    -- Setup the link properties
    props <- newLinkProperties linkProperties
    -- Create the link proxy
    pwLink <- pw_link_create core props
    cb pwLink
        `finally`
        -- Cleanup the proxy
        -- That does not seem necessary as the pw_core_disconnect takes care of that,
        -- but that's what the pw-link.c is doing.
        pw_proxy_destroy pwLink.getProxy

newLinkProperties :: LinkProperties -> IO PwProperties
newLinkProperties linkProperties = do
    props <- pw_properties_new
    pw_properties_set_id props "link.output.port" linkProperties.portOutput
    pw_properties_set_id props "link.input.port" linkProperties.portInput
    when linkProperties.linger do
        -- Keep the link after the program quit
        pw_properties_set_linger props
    pure props

pw_link_create :: PwCore -> PwProperties -> IO PwLink
pw_link_create core props =
    -- TODO: check for nullPtr
    PwLink <$> pw_core_create_object core "link-factory" "PipeWire:Interface:Link" mPW_VERSION_LINK props

newtype NodeID = NodeeID PwID
newtype PortID = PortID PwID

type LinkState = Either Text PwLinkState
type PwLinkEventInfoHandler = PwID -> LinkState -> IO ()

newtype PwLinkEvents = PwLinkEvents (Ptr PwLinkEventsStruct)

pwLinkEventsFuncs :: PwLinkEvents -> ProxiedFuncs
pwLinkEventsFuncs (PwLinkEvents pwe) = ProxiedFuncs $ castPtr pwe

withPwLinkEvents :: PwLinkEventInfoHandler -> (PwLinkEvents -> IO a) -> IO a
withPwLinkEvents infoHandler cb =
    allocaBytes
        (fromIntegral [C.pure| size_t {sizeof (struct pw_link_events)} |])
        \pwLinkEvents -> do
            infoP <- $(C.mkFunPtr [t|Ptr () -> Ptr PwLinkInfoStruct -> IO ()|]) wrapper
            [C.block| void{
                struct pw_link_events* ple = $(struct pw_link_events* pwLinkEvents);
                ple->version = PW_VERSION_LINK_EVENTS;
                ple->info = $(void (*infoP)(void*, const struct pw_link_info*));
        }|]
            cb (PwLinkEvents pwLinkEvents)
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

-- LEGACY, use withPwLinkEvents instead
with_pw_link_events :: PwLink -> PwLinkEventInfoHandler -> IO a -> IO a
with_pw_link_events (PwLink pwProxy) infoHandler cb =
    with_spa_hook \spaHook ->
        withPwLinkEvents infoHandler \ple -> do
            pw_proxy_add_object_listener pwProxy spaHook (pwLinkEventsFuncs ple)
            cb
