module Pipewire.SPA.Buffers where

import Pipewire.SPA.CContext
import Pipewire.Internal

newtype SpaBuffer = SpaBuffer (Ptr SpaBufferStruct)
