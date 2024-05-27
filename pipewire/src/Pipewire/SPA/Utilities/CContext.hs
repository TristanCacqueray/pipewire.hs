module Pipewire.SPA.Utilities.CContext where

import Data.Map.Strict qualified as Map
import Language.C.Inline.Context (Context (..))
import Language.C.Types (TypeSpecifier (Struct))

data SpaHookStruct

pwContext :: Context
pwContext =
    mempty
        { ctxTypesTable =
            Map.fromList
                [ (Struct "spa_hook", [t|SpaHookStruct|])
                ]
        }
