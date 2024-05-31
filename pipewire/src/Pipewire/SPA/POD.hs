module Pipewire.SPA.POD where

import Pipewire.Internal
import Pipewire.SPA.CContext

newtype SpaPod = SpaPod (Ptr SpaPodStruct)
