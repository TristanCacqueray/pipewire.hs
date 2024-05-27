module Pipewire.Utilities.CContext where

import Data.Map.Strict qualified as Map
import Foreign (Ptr)
import Language.C.Inline.Context (Context (..))
import Language.C.Types (TypeSpecifier (Struct))

newtype PwProperties = PwProperties (Ptr PwPropertiesStruct)
data PwPropertiesStruct

pwContext :: Context
pwContext =
    mempty
        { ctxTypesTable =
            Map.fromList
                [ (Struct "pw_properties", [t|PwPropertiesStruct|])
                ]
        }
