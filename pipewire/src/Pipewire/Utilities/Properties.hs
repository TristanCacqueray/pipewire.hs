module Pipewire.Utilities.Properties where

import Foreign (Ptr)
import Language.C.Inline qualified as C

import Pipewire.Utilities.CContext

C.context (C.baseCtx <> pwContext)

C.include "<pipewire/properties.h>"

newtype PwProperties = PwProperties (Ptr PwPropertiesStruct)

pw_properties_new :: IO PwProperties
pw_properties_new =
    PwProperties <$> [C.exp| struct pw_properties*{pw_properties_new(NULL, NULL)} |]
