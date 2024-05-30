module Pipewire.CoreAPI.CContext where

import Data.Map.Strict qualified as Map
import Language.C.Inline.Context (Context (..))
import Language.C.Types

data PwLoopStruct
data PwMainLoopStruct
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

pwContext :: Context
pwContext =
    mempty
        { ctxTypesTable =
            Map.fromList
                [ (Struct "pw_main_loop", [t|PwMainLoopStruct|])
                , (Struct "pw_loop", [t|PwLoopStruct|])
                , (Struct "pw_context", [t|PwContextStruct|])
                , (Struct "pw_core", [t|PwCoreStruct|])
                , (Struct "pw_registry", [t|PwRegistryStruct|])
                , (Struct "pw_registry_events", [t|PwRegistryEventsStruct|])
                , (Struct "pw_core_info", [t|PwCoreInfoStruct|])
                , (Struct "pw_core_events", [t|PwCoreEventsStruct|])
                , (Struct "pw_link_info", [t|PwLinkInfoStruct|])
                , (Struct "pw_link_events", [t|PwLinkEventsStruct|])
                , (Struct "pw_proxy_events", [t|PwLinkEventsStruct|])
                , (Struct "pw_proxy", [t|PwProxyStruct|])
                ]
        }
