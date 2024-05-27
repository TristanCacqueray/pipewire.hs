{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}

module Pipewire.Utilities.Properties where

import Language.C.Inline qualified as C

import Pipewire.Utilities.PropertiesCTX

C.context (C.baseCtx <> pwContext)

C.include "<pipewire/properties.h>"

pw_properties_new :: IO PwProperties
pw_properties_new =
    PwProperties <$> [C.exp| struct pw_properties*{pw_properties_new(NULL, NULL)} |]
