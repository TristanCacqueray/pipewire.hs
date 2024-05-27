module Pipewire.CoreCTX where

import Data.Map.Strict qualified as Map
import Language.C.Inline.Context (Context (..))
import Language.C.Types (TypeSpecifier (Struct))

data PwCoreEventsStruct
data PwCoreInfoStruct

pwContext :: Context
pwContext =
    mempty
        { ctxTypesTable =
            Map.fromList
                [ (Struct "pw_core_info", [t|PwCoreInfoStruct|])
                , (Struct "pw_core_events", [t|PwCoreEventsStruct|])
                ]
        }
