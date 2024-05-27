{- | The libpipewire structs.

Opaque pointer are wrapped with a newtype that contains a '*Struct' type which has no constructor.
Keep the list in sync with 'Pipewire.Context.pwContext'
-}
module Pipewire.Structs where

import Foreign (Ptr)
import Pipewire.Context

newtype PwLoop = PwLoop (Ptr PwLoopStruct)
newtype PwMainLoop = PwMainLoop (Ptr PwMainLoopStruct)
newtype PwContext = PwContext (Ptr PwContextStruct)
newtype PwCore = PwCore (Ptr PwCoreStruct)
newtype PwRegistry = PwRegistry (Ptr PwRegistryStruct)
newtype PwRegistryEvents = PwRegistryEvents (Ptr PwRegistryEventsStruct)
newtype SpaHook = SpaHook (Ptr SpaHookStruct)
newtype SpaDict = SpaDict (Ptr SpaDictStruct)
