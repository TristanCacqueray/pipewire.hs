module Pipewire.SPA.POD where

import Pipewire.Prelude
import Pipewire.SPA.CContext

newtype SpaPod = SpaPod (Ptr SpaPodStruct)
