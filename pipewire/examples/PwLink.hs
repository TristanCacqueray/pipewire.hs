-- | A minimal hs-pw-link implementation
module Main (main) where

import Control.Concurrent.STM.TChan (TChan, newTChanIO, tryReadTChan, writeTChan)
import Control.Monad (void, when)
import Control.Monad.STM (atomically)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import System.Environment (getArgs)

import Pipewire qualified as PW

data Options = ListInputs | ListOutputs

main :: IO ()
main =
    getArgs >>= \case
        ["list-inputs"] -> go ListInputs
        ["list-outputs"] -> go ListOutputs
        _ -> putStrLn "usage: hs-pw-link list-inputs|list-outputs|connect out in|disconnect pwid"
  where
    go options = withInstance \pwInstance -> do
        case options of
            ListInputs -> do
                syncInstance pwInstance
                forEachEvents pwInstance (printPorts "in")
            ListOutputs -> do
                syncInstance pwInstance
                forEachEvents pwInstance (printPorts "out")

    printPorts wantedDir pwid evtype props = do
        when (evtype == "PipeWire:Interface:Port") do
            PW.pw_properties_get props "port.direction" >>= \case
                Just dir | dir == wantedDir -> do
                    name <- getName props
                    putStrLn $ show pwid <> ": " <> Text.unpack name
                _ -> pure ()

    getName props = fromMaybe "N/A" <$> PW.pw_properties_get props "port.alias"

    forEachEvents pwInstance cb =
        atomically (tryReadTChan pwInstance.registryEvents) >>= \case
            Just ev -> do
                case ev of
                    Added pwid evtype props -> cb pwid evtype props
                    Removed _ -> pure ()
                forEachEvents pwInstance cb
            Nothing -> pure ()

-- | Here is a tentative high level API to implement a pipewire client
data PwInstance = PwInstance
    { registryEvents :: TChan RegistryEvent
    , mainLoop :: PW.PwMainLoop
    , core :: PW.PwCore
    , sync :: IORef PW.SeqID
    }

data RegistryEvent = Added PW.PwID Text PW.PwProperties | Removed PW.PwID

-- | Ensure all the events have been processed, including the initial registry received through the update handler
syncInstance :: PwInstance -> IO ()
syncInstance pwInstance = do
    -- Write the expected SeqID so that the core handler stop the loop
    writeIORef pwInstance.sync =<< PW.pw_core_sync pwInstance.core PW.pw_id_core
    -- Start the loop
    void $ PW.pw_main_loop_run pwInstance.mainLoop

withInstance :: (PwInstance -> IO a) -> IO a
withInstance cb =
    PW.withPipewire do
        PW.withMainLoop $ \mainLoop -> do
            loop <- PW.pw_main_loop_get_loop mainLoop
            PW.withContext loop \context -> do
                PW.withCore context \core -> do
                    sync <- newIORef (PW.SeqID 0)
                    PW.with_pw_core_events infoHandler (doneHandler mainLoop sync) errorHandler \coreEvents -> do
                        PW.with_spa_hook \coreListener -> do
                            PW.pw_core_add_listener core coreListener coreEvents
                            PW.with_spa_hook \registryListener -> do
                                chan <- newTChanIO
                                PW.with_pw_registry_events (handler chan) (removeHandler chan) \registryEvent -> do
                                    registry <- PW.pw_core_get_registry core
                                    PW.pw_registry_add_listener registry registryListener registryEvent
                                    cb PwInstance{registryEvents = chan, mainLoop, sync, core}
  where
    handler chan pwid name _ dict = do
        props <- PW.pw_properties_new_dict dict
        atomically $ writeTChan chan $ Added pwid name props
    removeHandler chan pwid = atomically $ writeTChan chan $ Removed pwid
    infoHandler _pwinfo = pure ()
    errorHandler pwid seq' res msg = do
        putStrLn $ "error: id:" <> show pwid <> " seq:" <> show seq' <> " res:" <> show res <> " message:" <> show msg
    doneHandler mainLoop sync _pwid seqid = do
        pending <- readIORef sync
        when (pending == seqid) do
            void $ PW.pw_main_loop_quit mainLoop
