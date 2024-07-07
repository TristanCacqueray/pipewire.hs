-- | Internal module for the inline-c structure mapping
module Pipewire.CContext where

import Data.Map.Strict qualified as Map
import Language.C.Inline.Context (Context (..))
import Language.C.Types

-- CoreAPI
data PwLoopStruct
data PwMainLoopStruct
data PwThreadLoopStruct
data PwContextStruct
data PwCoreStruct
data PwRegistryStruct
data PwRegistryEventsStruct
data PwProxyStruct

data PwCoreEventsStruct
data PwCoreInfoStruct

data PwProxyEventsStruct
data PwLinkEventsStruct
data PwLinkInfoStruct

data PwClientEventsStruct
data PwClientInfoStruct

data PwNodeEventsStruct
data PwNodeInfoStruct

data PwStreamStruct
data PwStreamEventsStruct
data PwBufferStruct

data SpaSourceStruct

-- Extensions
data PwMetadataEventsStruct

-- SPA
data SpaHookStruct
data SpaDictStruct
data SpaBufferStruct

data SpaPodStruct
data SpaVideoInfoRawStruct

-- Utilities
data PwPropertiesStruct

pwContext :: Context
pwContext =
    mempty
        { ctxTypesTable =
            Map.fromList
                [ (Struct "pw_main_loop", [t|PwMainLoopStruct|])
                , (Struct "pw_thread_loop", [t|PwThreadLoopStruct|])
                , (Struct "pw_loop", [t|PwLoopStruct|])
                , (Struct "pw_context", [t|PwContextStruct|])
                , (Struct "pw_core", [t|PwCoreStruct|])
                , (Struct "pw_registry", [t|PwRegistryStruct|])
                , (Struct "pw_registry_events", [t|PwRegistryEventsStruct|])
                , (Struct "pw_core_info", [t|PwCoreInfoStruct|])
                , (Struct "pw_core_events", [t|PwCoreEventsStruct|])
                , (Struct "pw_link_info", [t|PwLinkInfoStruct|])
                , (Struct "pw_link_events", [t|PwLinkEventsStruct|])
                , (Struct "pw_client_info", [t|PwClientInfoStruct|])
                , (Struct "pw_client_events", [t|PwClientEventsStruct|])
                , (Struct "pw_node_info", [t|PwNodeInfoStruct|])
                , (Struct "pw_node_events", [t|PwNodeEventsStruct|])
                , (Struct "pw_metadata_events", [t|PwMetadataEventsStruct|])
                , (Struct "pw_proxy_events", [t|PwProxyEventsStruct|])
                , (Struct "pw_proxy", [t|PwProxyStruct|])
                , (Struct "pw_stream", [t|PwStreamStruct|])
                , (Struct "pw_stream_events", [t|PwStreamEventsStruct|])
                , (Struct "pw_buffer", [t|PwBufferStruct|])
                , (Struct "spa_source", [t|SpaSourceStruct|])
                , (Struct "spa_hook", [t|SpaHookStruct|])
                , (Struct "spa_dict", [t|SpaDictStruct|])
                , (Struct "spa_buffer", [t|SpaBufferStruct|])
                , (Struct "spa_pod", [t|SpaPodStruct|])
                , (Struct "spa_video_info_raw", [t|SpaVideoInfoRawStruct|])
                , (Struct "pw_properties", [t|PwPropertiesStruct|])
                ]
        }
