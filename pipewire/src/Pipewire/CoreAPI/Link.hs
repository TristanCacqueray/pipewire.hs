module Pipewire.CoreAPI.Link where

import Foreign (castPtr)
import Language.C.Inline qualified as C

import Pipewire.CContext
import Pipewire.CoreAPI.Core
import Pipewire.CoreAPI.Proxy
import Pipewire.Enum
import Pipewire.Prelude
import Pipewire.Protocol (PwID (..))
import Pipewire.Utilities.Properties (PwProperties, pw_properties_new, pw_properties_set_id, pw_properties_set_linger)

C.context (C.baseCtx <> pwContext)

C.include "<pipewire/link.h>"

-- | A Link proxy
newtype Link = Link {getProxy :: PwProxy}

-- | Link creation options
data LinkProperties = LinkProperties
    { portOutput :: PwID
    , portInput :: PwID
    , linger :: Bool
    -- ^ Set to True to keep the link after the program quit
    }

-- | Setup a link (synchronously)
withLink :: Core -> LinkProperties -> (Link -> IO a) -> IO a
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

-- | Create the PwProperties for 'pw_link_create'
newLinkProperties :: LinkProperties -> IO PwProperties
newLinkProperties linkProperties = do
    props <- pw_properties_new
    pw_properties_set_id props "link.output.port" linkProperties.portOutput
    pw_properties_set_id props "link.input.port" linkProperties.portInput
    when linkProperties.linger do
        -- Keep the link after the program quit
        pw_properties_set_linger props
    pure props

pw_link_create :: Core -> PwProperties -> IO Link
pw_link_create core props =
    -- TODO: check for nullPtr
    Link <$> pw_core_create_object core "link-factory" "PipeWire:Interface:Link" mPW_VERSION_LINK props

newtype NodeID = NodeeID PwID
newtype PortID = PortID PwID

type LinkEventInfoHandler = PwID -> Either Text LinkState -> IO ()

newtype LinkEvents = LinkEvents (Ptr PwLinkEventsStruct)

-- | Convert the 'LinkEvents' for 'pw_proxy_add_object_listener'
pwLinkEventsFuncs :: LinkEvents -> ProxiedFuncs
pwLinkEventsFuncs (LinkEvents pwe) = ProxiedFuncs $ castPtr pwe

-- | Setup the pw_link_events handlers
withLinkEvents :: LinkEventInfoHandler -> (LinkEvents -> IO a) -> IO a
withLinkEvents infoHandler cb =
    allocaBytes
        (fromIntegral [C.pure| size_t {sizeof (struct pw_link_events)} |])
        \pwLinkEvents -> do
            infoP <- $(C.mkFunPtr [t|Ptr () -> Ptr PwLinkInfoStruct -> IO ()|]) wrapper
            [C.block| void{
                struct pw_link_events* ple = $(struct pw_link_events* pwLinkEvents);
                ple->version = PW_VERSION_LINK_EVENTS;
                ple->info = $(void (*infoP)(void*, const struct pw_link_info*));
        }|]
            cb (LinkEvents pwLinkEvents)
                `finally` do
                    freeHaskellFunPtr infoP
  where
    wrapper _data ptr = do
        pwid <- PwID . fromIntegral <$> [C.exp| int{$(struct pw_link_info* ptr)->id}|]
        (state :: LinkState) <- LinkState <$> [C.exp| int{$(struct pw_link_info* ptr)->state} |]
        case state of
            PW_LINK_STATE_ERROR -> do
                (errC :: CString) <- [C.exp| const char*{$(struct pw_link_info* ptr)->error} |]
                err <- peekCString errC
                infoHandler pwid (Left err)
            _ -> infoHandler pwid (Right state)
