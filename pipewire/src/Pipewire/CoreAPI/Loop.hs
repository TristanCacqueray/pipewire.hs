module Pipewire.CoreAPI.Loop where

import Foreign (Ptr)
import Pipewire.CoreAPI.CContext

newtype PwLoop = PwLoop (Ptr PwLoopStruct)
