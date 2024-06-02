module Pipewire.SPA.Buffers where

import Pipewire.Prelude
import Pipewire.SPA.CContext

newtype SpaBuffer = SpaBuffer (Ptr SpaBufferStruct)
