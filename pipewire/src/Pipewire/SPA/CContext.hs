module Pipewire.SPA.CContext where

import Data.Map.Strict qualified as Map
import Language.C.Inline.Context (Context (..))
import Language.C.Types (TypeSpecifier (Struct))

data SpaHookStruct
data SpaDictStruct
data SpaBufferStruct

data SpaPodStruct
data SpaVideoInfoRawStruct

pwContext :: Context
pwContext =
    mempty
        { ctxTypesTable =
            Map.fromList
                [ (Struct "spa_hook", [t|SpaHookStruct|])
                , (Struct "spa_dict", [t|SpaDictStruct|])
                , (Struct "spa_buffer", [t|SpaBufferStruct|])
                , (Struct "spa_pod", [t|SpaPodStruct|])
                , (Struct "spa_video_info_raw", [t|SpaVideoInfoRawStruct|])
                ]
        }
