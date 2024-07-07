module Pipewire.CoreAPI.Device where

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
newtype Device = Device {getProxy :: PwProxy}

-- Q: does this needs to be freed?
--
-- Note: a couple of spa_hook are requested for the user data,
-- they are used to setup the device listener and the proxy events
-- See the '&hooks[1]' below.
-- This lets use avoid managing spa_hook allocation in callbacks.
bindDevice :: Registry -> PwID -> IO Device
bindDevice (Registry registry) (PwID (fromIntegral -> pwid)) =
    Device . PwProxy <$> dieOnNull "pw_registry_bind" do
        [C.exp|struct pw_proxy* {pw_registry_bind(
        $(struct pw_registry* registry)
      , $(uint32_t pwid)
      , PW_TYPE_INTERFACE_Device
      , PW_VERSION_DEVICE
      , 2 * sizeof (struct spa_hook)
  )}|]

type DeviceInfoHandler = PwID -> SpaDict -> IO ()

withDeviceInfoHandler :: DeviceInfoHandler -> DeviceEvents -> IO a -> IO a
withDeviceInfoHandler handler (DeviceEvents deviceEvents) cb = do
    infoP <- $(C.mkFunPtr [t|Ptr () -> Ptr PwDeviceInfoStruct -> IO ()|]) wrapper
    [C.block|void{
       struct pw_device_events* events = $(struct pw_device_events* deviceEvents);
       // TODO: write this when creating the DeviceEvents struct
       events->version = PW_VERSION_DEVICE_EVENTS;
       events->info = $(void (*infoP)(void*, const struct pw_device_info*));
    }|]
    cb `finally` freeHaskellFunPtr infoP
  where
    wrapper _data info = do
        props <- [C.exp|struct spa_dict*{ $(struct pw_device_info* info)->props }|]
        pwid <- PwID . fromIntegral <$> [C.exp|uint32_t{ $(struct pw_device_info* info)->id }|]
        handler pwid (SpaDict props)

addDeviceListener :: Device -> DeviceEvents -> IO ()
addDeviceListener (Device (PwProxy device)) (DeviceEvents deviceEvents) =
    [C.block|void{
    struct pw_proxy* proxy = $(struct pw_proxy* device);
    struct spa_hook* hooks = pw_proxy_get_user_data(proxy);

    pw_proxy_add_object_listener(proxy, &hooks[1], $(struct pw_device_events* deviceEvents), NULL);
  }|]

newtype DeviceEvents = DeviceEvents (Ptr PwDeviceEventsStruct)

withDeviceEvents :: (DeviceEvents -> IO a) -> IO a
withDeviceEvents cb = do
    allocaBytes
        (fromIntegral [C.pure| size_t {sizeof (struct pw_device_events)} |])
        \pwDeviceEvents -> do
            -- TODO: write version number
            cb (DeviceEvents pwDeviceEvents)
