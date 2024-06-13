module Pipewire.SPA.Buffers where

import Pipewire.CContext
import Pipewire.Prelude

newtype SpaBuffer = SpaBuffer (Ptr SpaBufferStruct)
