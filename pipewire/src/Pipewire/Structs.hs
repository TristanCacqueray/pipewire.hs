{- | The libpipewire structs.

Opaque pointer are wrapped with a newtype that contains a '*Struct' type which has no constructor.
Keep the list in sync with 'Pipewire.Context.pwContext'
-}
module Pipewire.Structs where

import Foreign (Ptr)

newtype PwLoop = PwLoop (Ptr PwLoopStruct)
data PwLoopStruct

newtype PwMainLoop = PwMainLoop (Ptr PwMainLoopStruct)
data PwMainLoopStruct

newtype PwContext = PwContext (Ptr PwContextStruct)
data PwContextStruct

newtype PwCore = PwCore (Ptr PwCoreStruct)
data PwCoreStruct

newtype PwRegistry = PwRegistry (Ptr PwRegistryStruct)
data PwRegistryStruct

newtype PwRegistryEvents = PwRegistryEvents (Ptr PwRegistryEventsStruct)
data PwRegistryEventsStruct

newtype SpaHook = SpaHook (Ptr SpaHookStruct)
data SpaHookStruct

newtype SpaDict = SpaDict (Ptr SpaDictStruct)
data SpaDictStruct
