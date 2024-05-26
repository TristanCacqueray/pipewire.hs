-- | This modules provides the inline-c context to convert C types to Haskell
module Pipewire.Context where

import Data.Map.Strict qualified as Map
import Language.C.Inline.Context (Context (..))
import Language.C.Types

import Pipewire.Structs

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
                , (Struct "spa_hook", [t|SpaHookStruct|])
                , (Struct "spa_dict", [t|SpaDictStruct|])
                ]
        }
