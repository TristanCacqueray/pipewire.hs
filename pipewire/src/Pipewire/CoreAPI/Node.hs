module Pipewire.CoreAPI.Node where

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
newtype Node = Node {getProxy :: PwProxy}

-- Q: does this needs to be freed?k
--
-- Note: a couple of spa_hook are requested for the user data,
-- they are used to setup the node listener and the proxy events
-- See the '&hooks[1]' below.
-- This lets use avoid managing spa_hook allocation in callbacks.
bindNode :: Registry -> PwID -> IO Node
bindNode (Registry registry) (PwID (fromIntegral -> pwid)) =
    Node . PwProxy <$> dieOnNull "pw_registry_bind" do
        [C.exp|struct pw_proxy* {pw_registry_bind(
        $(struct pw_registry* registry)
      , $(uint32_t pwid)
      , PW_TYPE_INTERFACE_Node
      , PW_VERSION_NODE
      , 2 * sizeof (struct spa_hook)
  )}|]

type NodeInfoHandler = PwID -> SpaDict -> IO ()

withNodeInfoHandler :: NodeInfoHandler -> NodeEvents -> IO a -> IO a
withNodeInfoHandler handler (NodeEvents nodeEvents) cb = do
    infoP <- $(C.mkFunPtr [t|Ptr () -> Ptr PwNodeInfoStruct -> IO ()|]) wrapper
    [C.block|void{
       struct pw_node_events* events = $(struct pw_node_events* nodeEvents);
       // TODO: write this when creating the NodeEvents struct
       events->version = PW_VERSION_NODE_EVENTS;
       events->info = $(void (*infoP)(void*, const struct pw_node_info*));
    }|]
    cb `finally` freeHaskellFunPtr infoP
  where
    wrapper _data info = do
        props <- [C.exp|struct spa_dict*{ $(struct pw_node_info* info)->props }|]
        pwid <- PwID . fromIntegral <$> [C.exp|uint32_t{ $(struct pw_node_info* info)->id }|]
        handler pwid (SpaDict props)

addNodeListener :: Node -> NodeEvents -> IO ()
addNodeListener (Node (PwProxy node)) (NodeEvents nodeEvents) =
    [C.block|void{
    struct pw_proxy* proxy = $(struct pw_proxy* node);
    struct spa_hook* hooks = pw_proxy_get_user_data(proxy);

    pw_proxy_add_object_listener(proxy, &hooks[1], $(struct pw_node_events* nodeEvents), NULL);
  }|]

newtype NodeEvents = NodeEvents (Ptr PwNodeEventsStruct)

withNodeEvents :: (NodeEvents -> IO a) -> IO a
withNodeEvents cb = do
    allocaBytes
        (fromIntegral [C.pure| size_t {sizeof (struct pw_node_events)} |])
        \pwNodeEvents -> do
            -- TODO: write version number
            cb (NodeEvents pwNodeEvents)
