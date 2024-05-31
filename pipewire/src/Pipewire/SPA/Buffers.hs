module Pipewire.SPA.Buffers where

import Pipewire.Internal
import Pipewire.SPA.CContext

newtype SpaBuffer = SpaBuffer (Ptr SpaBufferStruct)
