module Pipewire.Utilities.CContext where

import Data.Map.Strict qualified as Map
import Language.C.Inline.Context (Context (..))
import Language.C.Types (TypeSpecifier (Struct))

data PwPropertiesStruct

pwContext :: Context
pwContext =
    mempty
        { ctxTypesTable =
            Map.fromList
                [ (Struct "pw_properties", [t|PwPropertiesStruct|])
                ]
        }
