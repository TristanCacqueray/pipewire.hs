module Pipewire.SPA.POD where

import Pipewire.CContext
import Pipewire.Prelude

newtype SpaPod = SpaPod (Ptr SpaPodStruct)
