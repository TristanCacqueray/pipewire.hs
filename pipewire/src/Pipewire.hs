module Pipewire (
    -- * High level API
    withPipewire,
    withMainLoop,
    withContext,
    withCore,

    -- * Protocol
    module Pipewire.Protocol,
    module Pipewire.Constants,

    -- * Core API

    -- ** Initialization
    module Pipewire.CoreAPI.Initialization,

    -- ** Main Loop
    module Pipewire.CoreAPI.MainLoop,

    -- ** Context
    module Pipewire.CoreAPI.Context,

    -- ** Core
    module Pipewire.CoreAPI.Core,

    -- ** Loop
    module Pipewire.CoreAPI.Loop,

    -- ** Registry
    module Pipewire.CoreAPI.Registry,

    -- * Utilities

    -- ** Properties
    module Pipewire.Utilities.Properties,

    -- * SPA

    -- ** Utilities

    -- *** Dictionary
    module Pipewire.SPA.Utilities.Dictionary,

    -- *** Hooks
    module Pipewire.SPA.Utilities.Hooks,

    -- * Helpers
    getHeadersVersion,
    getLibraryVersion,
)
where

import Control.Exception (bracket, bracket_)
import Language.C.Inline qualified as C

import Pipewire.Constants
import Pipewire.CoreAPI.Context (PwContext, pw_context_connect, pw_context_destroy, pw_context_new)
import Pipewire.CoreAPI.Core (DoneHandler, ErrorHandler, InfoHandler, PwCore, PwCoreEvents, PwCoreInfo, pw_core_add_listener, pw_core_disconnect, pw_core_get_registry, pw_core_sync, pw_id_core, with_pw_core_events)
import Pipewire.CoreAPI.Initialization (pw_deinit, pw_init)
import Pipewire.CoreAPI.Loop (PwLoop)
import Pipewire.CoreAPI.MainLoop (PwMainLoop, pw_main_loop_destroy, pw_main_loop_get_loop, pw_main_loop_new, pw_main_loop_quit, pw_main_loop_run)
import Pipewire.CoreAPI.Registry (GlobalHandler, GlobalRemoveHandler, pw_registry_add_listener, with_pw_registry_events)
import Pipewire.Internal
import Pipewire.Protocol (PwID (..), PwVersion (..), SeqID (..))
import Pipewire.SPA.Utilities.Dictionary (SpaDict, spaDictLookup, spaDictRead)
import Pipewire.SPA.Utilities.Hooks (SpaHook, with_spa_hook)
import Pipewire.Utilities.Properties (PwProperties, pw_properties_get, pw_properties_new, pw_properties_new_dict, pw_properties_set)

C.include "<pipewire/pipewire.h>"

withPipewire :: IO a -> IO a
withPipewire = bracket_ pw_init pw_deinit

withMainLoop :: (PwMainLoop -> IO a) -> IO a
withMainLoop = bracket pw_main_loop_new pw_main_loop_destroy

withContext :: PwLoop -> (PwContext -> IO a) -> IO a
withContext loop = bracket (pw_context_new loop) pw_context_destroy

withCore :: PwContext -> (PwCore -> IO a) -> IO a
withCore context = bracket (pw_context_connect context) pw_core_disconnect

getHeadersVersion :: IO Text
getHeadersVersion = ([C.exp| const char*{pw_get_headers_version()} |] :: IO CString) >>= peekCString

getLibraryVersion :: IO Text
getLibraryVersion = ([C.exp| const char*{pw_get_library_version()} |] :: IO CString) >>= peekCString
