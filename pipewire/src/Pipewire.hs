module Pipewire (
    -- * High level API
    PwInstance (..),
    RegistryEvent (..),
    withInstance,
    withThreadedInstance,
    withRegistryHandler,
    runInstance,
    CoreError (..),
    syncState,
    syncState_,
    readState,

    -- * Mid level bracket API
    withPipewire,
    withMainLoop,
    withThreadLoop,
    withContext,
    withCore,

    -- * Protocol
    module Pipewire.Protocol,
    module Pipewire.Enum,

    -- * Core API

    -- ** Initialization
    module Pipewire.CoreAPI.Initialization,

    -- ** Main Loop
    module Pipewire.CoreAPI.MainLoop,

    -- ** Thread Loop
    module Pipewire.CoreAPI.ThreadLoop,

    -- ** Context
    module Pipewire.CoreAPI.Context,

    -- ** Core
    module Pipewire.CoreAPI.Core,

    -- ** Link
    module Pipewire.CoreAPI.Link,
    waitForLink,
    waitForLinkAsync,

    -- ** Loop
    module Pipewire.CoreAPI.Loop,

    -- ** Node
    module Pipewire.CoreAPI.Node,

    -- ** Proxy
    module Pipewire.CoreAPI.Proxy,

    -- ** Registry
    module Pipewire.CoreAPI.Registry,

    -- * Utilities

    -- ** Properties
    module Pipewire.Utilities.Properties,

    -- * Extensions
    module Pipewire.Extensions.Metadata,
    withMetadata,

    -- * SPA

    -- ** Utilities

    -- *** Dictionary
    module Pipewire.SPA.Utilities.Dictionary,

    -- *** Hooks
    module Pipewire.SPA.Utilities.Hooks,

    -- * SPA
    module Pipewire.Stream,

    -- * Helpers
    getHeadersVersion,
    getLibraryVersion,
    cfloatVector,
)
where

import Control.Exception (bracket, bracket_)
import Language.C.Inline qualified as C

import Control.Concurrent (MVar, modifyMVar_, newMVar, readMVar)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
import Pipewire.CoreAPI.Context (Context, pw_context_connect, pw_context_destroy, pw_context_new)
import Pipewire.CoreAPI.Core (Core, CoreEvents, CoreInfo, DoneHandler, ErrorHandler, InfoHandler, Registry, pw_core_add_listener, pw_core_disconnect, pw_core_get_registry, pw_core_sync, pw_id_core, withCoreEvents)
import Pipewire.CoreAPI.Initialization (pw_deinit, pw_init)
import Pipewire.CoreAPI.Link (Link (..), LinkProperties (..), pwLinkEventsFuncs, pw_link_create, setupLinkProperties, withLink, withLinkEvents)
import Pipewire.CoreAPI.Loop (Loop)
import Pipewire.CoreAPI.MainLoop (MainLoop, pw_main_loop_destroy, pw_main_loop_get_loop, pw_main_loop_new, pw_main_loop_quit, pw_main_loop_run, withSignalsHandler)
import Pipewire.CoreAPI.Node (Node, NodeInfoHandler, withNodeEvents, withNodeInfoHandler)
import Pipewire.CoreAPI.Node qualified as PwNode
import Pipewire.CoreAPI.Proxy (PwProxy, pw_proxy_add_object_listener, pw_proxy_destroy, withProxyEvents)
import Pipewire.CoreAPI.Registry (GlobalHandler, GlobalRemoveHandler, pw_registry_add_listener, pw_registry_destroy, withRegistryEvents)
import Pipewire.CoreAPI.ThreadLoop (ThreadLoop, pw_thread_loop_destroy, pw_thread_loop_get_loop, pw_thread_loop_new, pw_thread_loop_signal, pw_thread_loop_start, pw_thread_loop_stop, pw_thread_loop_wait, withLock, withThreadSignalsHandler, withUnlock)
import Pipewire.Enum
import Pipewire.Extensions.Metadata
import Pipewire.Prelude
import Pipewire.Protocol (PwID (..), PwVersion (..), SeqID (..))
import Pipewire.SPA.Utilities.Dictionary (SpaDict, spaDictLookup, spaDictLookupInt, spaDictRead, spaDictSize, withSpaDict)
import Pipewire.SPA.Utilities.Hooks (SpaHook, withSpaHook)
import Pipewire.Stream (pw_stream_get_node_id)
import Pipewire.Utilities.Properties (PwProperties, pw_properties_free, pw_properties_get, pw_properties_new, pw_properties_new_dict, pw_properties_set, pw_properties_set_id, pw_properties_set_linger, withProperties)

C.include "<pipewire/pipewire.h>"

withPipewire :: IO a -> IO a
withPipewire = bracket_ pw_init pw_deinit

-- | Setup a main loop with signal handlers
withMainLoop :: (MainLoop -> IO a) -> IO a
withMainLoop cb = bracket pw_main_loop_new pw_main_loop_destroy withHandler
  where
    withHandler mainLoop = withSignalsHandler mainLoop (cb mainLoop)

withThreadLoop :: Text -> (ThreadLoop -> IO a) -> IO a
withThreadLoop name cb = withCString name \cname ->
    bracket (pw_thread_loop_new cname) pw_thread_loop_destroy withHandler
  where
    withHandler threadLoop = withThreadSignalsHandler threadLoop (cb threadLoop)

withContext :: Loop -> (Context -> IO a) -> IO a
withContext loop = bracket (pw_context_new loop) pw_context_destroy

withCore :: Context -> (Core -> IO a) -> IO a
withCore context = bracket (pw_context_connect context) pw_core_disconnect

getHeadersVersion :: IO Text
getHeadersVersion = ([C.exp| const char*{pw_get_headers_version()} |] :: IO CString) >>= peekCString

getLibraryVersion :: IO Text
getLibraryVersion = ([C.exp| const char*{pw_get_library_version()} |] :: IO CString) >>= peekCString

-- | A pipewire client instance
data PwInstance state = PwInstance
    { stateVar :: MVar state
    , instanceLoop :: InstanceLoop
    , core :: Core
    , registry :: Registry
    , sync :: MVar (Maybe SeqID)
    -- ^ the core sync pending value for the done handler
    -- when it is Nothing, don't quit the loop
    , errorsVar :: MVar [CoreError]
    }

data InstanceLoop = SyncLoop MainLoop | AsyncLoop ThreadLoop

stopInstanceLoop :: InstanceLoop -> IO ()
stopInstanceLoop = \case
    SyncLoop mainLoop -> pw_main_loop_quit mainLoop
    AsyncLoop threadLoop -> pw_thread_loop_signal threadLoop False

-- | A pipewire error
data CoreError = CoreError
    { pwid :: PwID
    , code :: Int
    , message :: Text
    }
    deriving (Show)

-- | A registry event
data RegistryEvent = Added PwID Text SpaDict | Removed PwID | ChangedNode PwID SpaDict

-- TODO: handle pw_main_loop error

-- | Run the main loop
runInstance :: PwInstance state -> IO (Maybe (NonEmpty CoreError))
runInstance pwInstance = do
    case pwInstance.instanceLoop of
        SyncLoop mainLoop -> pw_main_loop_run mainLoop
        AsyncLoop threadLoop -> withLock threadLoop $ pw_thread_loop_wait threadLoop
    getErrors pwInstance

readState :: PwInstance state -> IO state
readState pwInstance = readMVar pwInstance.stateVar

-- | Terminate the main loop, to be called from handlers.
quitInstance :: PwInstance state -> IO ()
quitInstance pwInstance = case pwInstance.instanceLoop of
    SyncLoop mainLoop -> pw_main_loop_quit mainLoop
    AsyncLoop threadLoop ->
        -- Question: do we need to call pw_thread_loop_signal ?
        pw_thread_loop_stop threadLoop

-- | Like 'syncState' but throwing an error if there was any pipewire error.
syncState_ :: PwInstance state -> IO state
syncState_ pwInstance =
    syncState pwInstance >>= \case
        Left errs -> mapM_ print errs >> error "pw core failed"
        Right state -> pure state

getErrors :: PwInstance state -> IO (Maybe (NonEmpty CoreError))
getErrors pwInstance = NE.nonEmpty <$> readMVar pwInstance.errorsVar

{- | Ensure all the events have been processed and access the state.
Do not call when the loop is runnning!
-}
syncState :: PwInstance state -> IO (Either (NonEmpty CoreError) state)
syncState pwInstance = do
    -- Start the loop
    case pwInstance.instanceLoop of
        SyncLoop mainLoop -> startSync >> pw_main_loop_run mainLoop
        AsyncLoop threadLoop -> withLock threadLoop do
            startSync >> pw_thread_loop_wait threadLoop
    -- Call back with the finalized state
    getErrors pwInstance >>= \case
        Just errs -> pure (Left errs)
        Nothing -> Right <$> readState pwInstance
  where
    -- Write a Some SeqID to the pending sync
    startSync =
        modifyMVar_ pwInstance.sync (fmap Just . pw_core_sync pwInstance.core pw_id_core)

-- | Create a new 'PwInstance'. Use 'withThreadedInstance' to use the 'PwInstance' with multiple thread.
withInstance :: state -> (MainLoop -> PwInstance state -> IO a) -> IO a
withInstance initialState cb = withPipewire do
    withMainLoop $ \mainLoop -> do
        loop <- pw_main_loop_get_loop mainLoop
        withInstanceLoop (SyncLoop mainLoop) loop initialState (cb mainLoop)

{- | Create a new 'PwInstance' using a threaded loop.
Note: the loop is already locked, use 'withRegistryHandler' or 'withUnlock' to let it run.
-}
withThreadedInstance :: state -> (ThreadLoop -> PwInstance state -> IO a) -> IO a
withThreadedInstance initialState cb =
    withPipewire do
        withThreadLoop "pipewire.hs" $ \threadLoop -> withLock threadLoop do
            pw_thread_loop_start threadLoop
            loop <- pw_thread_loop_get_loop threadLoop
            withInstanceLoop (AsyncLoop threadLoop) loop initialState \pwInstance -> do
                cb threadLoop pwInstance

withInstanceLoop :: InstanceLoop -> Loop -> state -> (PwInstance state -> IO a) -> IO a
withInstanceLoop instanceLoop loop initialState cb = do
    withContext loop \context -> do
        withCore context \core -> do
            sync <- newMVar Nothing
            errorsVar <- newMVar []
            withCoreEvents infoHandler (doneHandler sync) (errorHandler errorsVar) \coreEvents -> do
                withSpaHook \coreListener -> do
                    pw_core_add_listener core coreListener coreEvents

                    stateVar <- newMVar initialState
                    registry <- pw_core_get_registry core
                    let pwInstance = PwInstance{stateVar, errorsVar, instanceLoop, sync, core, registry}
                    cb pwInstance
  where
    infoHandler _pwinfo = pure ()
    errorHandler errorVar pwid _seq' res msg = modifyMVar_ errorVar (\xs -> pure $ CoreError pwid res msg : xs)
    doneHandler sync _pwid seqid = do
        modifyMVar_ sync \case
            -- syncState is running and we reached the pending value, quit the loop now
            Just pending
                | pending == seqid -> stopInstanceLoop instanceLoop >> pure Nothing
            -- we are not waiting for the loop to stop, so we don't stop the loop
            x -> pure x

{- | Setup 'RegistryEvents'. The loop must be already locked, as provided by 'withThreadedInstance'.
The closure is executed without the lock.
-}
withRegistryHandler :: PwInstance state -> (RegistryEvent -> IO ()) -> IO a -> IO a
withRegistryHandler pwInstance registryHandler go =
    -- Setup registry handlers
    withSpaHook \registryListener -> do
        withNodeEvents \nodeEvents -> withNodeInfoHandler nodeInfoHandler nodeEvents do
            withRegistryEvents (handler nodeEvents) removeHandler \registryEvent -> do
                pw_registry_add_listener pwInstance.registry registryListener registryEvent
                case pwInstance.instanceLoop of
                    AsyncLoop threadLoop -> withUnlock threadLoop go
                    _ -> go
  where
    removeHandler pwid = registryHandler $ Removed pwid
    nodeInfoHandler pwid props = do
        spaDictSize props >>= \case
            0 -> pure ()
            _ -> registryHandler $ ChangedNode pwid props
    handler nodeEvents pwid name _ props = do
        case name of
            "PipeWire:Interface:Node" -> do
                node <- PwNode.bindNode pwInstance.registry pwid
                -- Keep track of node params to get media change
                PwNode.addNodeListener node nodeEvents
                modifyMVar_ pwInstance.sync \case
                    -- syncState is not running
                    Nothing -> pure Nothing
                    -- update the pending sync to wait for node events to be processed
                    sync -> Just <$> pw_core_sync pwInstance.core pw_id_core sync
            _ -> pure ()
        registryHandler $ Added pwid name props

-- | Wait for link with a paused 'MainLoop'.
waitForLink :: Link -> PwInstance state -> IO (Maybe (NonEmpty CoreError))
waitForLink pwLink pwInstance = do
    let abort msg = putStrLn msg >> quitInstance pwInstance
        destroyHandler = abort "Destroyed!"
        removedHandler = abort "Proxy Removed!"
        errorHandler res err = abort $ "error: " <> show res <> " " <> show err
    withProxyEvents pwLink.getProxy destroyHandler removedHandler errorHandler do
        let infoHandler pwid state = case state of
                Left err -> abort $ "Link state failed: " <> show err
                Right PW_LINK_STATE_ACTIVE -> do
                    putStrLn "Link is active, quiting the loop!"
                    quitInstance pwInstance
                Right x -> do
                    putStrLn $ "Link state pwid " <> show pwid <> ": " <> show x

        withSpaHook \spaHook ->
            withLinkEvents infoHandler \ple -> do
                pw_proxy_add_object_listener pwLink.getProxy spaHook (pwLinkEventsFuncs ple)
                putStrLn "Waiting for link..."
                runInstance pwInstance

type LinkError = (Int, Text)

-- | Wait for link with a running 'ThreadLoop'
waitForLinkAsync :: Link -> ThreadLoop -> IO (Maybe (Either LinkError Text))
waitForLinkAsync pwLink threadLoop = do
    baton <- newEmptyMVar
    let abort msg = print msg >> putMVar baton (Just $ Right msg)
        destroyHandler = abort "link destroyed!"
        removedHandler = abort "link removed!"
        errorHandler res err = putMVar baton $ Just $ Left (res, err)
    withProxyEvents pwLink.getProxy destroyHandler removedHandler errorHandler do
        let infoHandler pwid state = case state of
                Left err -> do
                    putStrLn $ "Link error: " <> show err
                    putMVar baton $ Just $ Right err
                Right PW_LINK_STATE_ACTIVE -> do
                    putStrLn "Link is active, quiting the loop!"
                    putMVar baton Nothing
                Right PW_LINK_STATE_INIT -> do
                    putStrLn "Link is initialized, we can stop as this can be the final state for a paused connection"
                    putMVar baton Nothing
                Right x -> do
                    putStrLn $ "Link state pwid " <> show pwid <> ": " <> show x

        withSpaHook \spaHook ->
            withLinkEvents infoHandler \ple -> do
                pw_proxy_add_object_listener pwLink.getProxy spaHook (pwLinkEventsFuncs ple)
                withUnlock threadLoop $ takeMVar baton

withMetadata :: Registry -> PwID -> (Metadata -> IO a) -> IO a
withMetadata registry pwid = bracket (bindMetadata registry pwid) (\m -> pw_proxy_destroy m.getProxy)
