module Pipewire.CoreAPI.Client where

import Language.C.Inline qualified as C
import Pipewire.CContext
import Pipewire.CoreAPI.Core (Registry (..))
import Pipewire.CoreAPI.Proxy
import Pipewire.Prelude
import Pipewire.Protocol (PwID (..))
import Pipewire.SPA.Utilities.Dictionary (SpaDict (..))

C.context (C.baseCtx <> pwContext)

C.include "<pipewire/pipewire.h>"

-- | A Link proxy
newtype Client = Client {getProxy :: PwProxy}

-- Q: does this needs to be freed?
--
-- Note: a couple of spa_hook are requested for the user data,
-- they are used to setup the client listener and the proxy events
-- See the '&hooks[1]' below.
-- This lets use avoid managing spa_hook allocation in callbacks.
bindClient :: Registry -> PwID -> IO Client
bindClient (Registry registry) (PwID (fromIntegral -> pwid)) =
    Client . PwProxy <$> dieOnNull "pw_registry_bind" do
        [C.exp|struct pw_proxy* {pw_registry_bind(
        $(struct pw_registry* registry)
      , $(uint32_t pwid)
      , PW_TYPE_INTERFACE_Client
      , PW_VERSION_CLIENT
      , 2 * sizeof (struct spa_hook)
  )}|]

type ClientInfoHandler = PwID -> SpaDict -> IO ()

withClientInfoHandler :: ClientInfoHandler -> ClientEvents -> IO a -> IO a
withClientInfoHandler handler (ClientEvents clientEvents) cb = do
    infoP <- $(C.mkFunPtr [t|Ptr () -> Ptr PwClientInfoStruct -> IO ()|]) wrapper
    [C.block|void{
       struct pw_client_events* events = $(struct pw_client_events* clientEvents);
       // TODO: write this when creating the ClientEvents struct
       events->version = PW_VERSION_CLIENT_EVENTS;
       events->info = $(void (*infoP)(void*, const struct pw_client_info*));
    }|]
    cb `finally` freeHaskellFunPtr infoP
  where
    wrapper _data info = do
        props <- [C.exp|struct spa_dict*{ $(struct pw_client_info* info)->props }|]
        pwid <- PwID . fromIntegral <$> [C.exp|uint32_t{ $(struct pw_client_info* info)->id }|]
        handler pwid (SpaDict props)

addClientListener :: Client -> ClientEvents -> IO ()
addClientListener (Client (PwProxy client)) (ClientEvents clientEvents) =
    [C.block|void{
    struct pw_proxy* proxy = $(struct pw_proxy* client);
    struct spa_hook* hooks = pw_proxy_get_user_data(proxy);

    pw_proxy_add_object_listener(proxy, &hooks[1], $(struct pw_client_events* clientEvents), NULL);
  }|]

newtype ClientEvents = ClientEvents (Ptr PwClientEventsStruct)

withClientEvents :: (ClientEvents -> IO a) -> IO a
withClientEvents cb = do
    allocaBytes
        (fromIntegral [C.pure| size_t {sizeof (struct pw_client_events)} |])
        \pwClientEvents -> do
            -- TODO: write version number
            cb (ClientEvents pwClientEvents)
