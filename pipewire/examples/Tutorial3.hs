module Main (main) where

import Data.IORef
import Control.Monad (when)

import Pipewire qualified as PW
import Pipewire.Raw qualified as Raw
import Pipewire.Protocol

import Pipewire.Core qualified as PwCore

main :: IO ()
main =
    PW.withPipewire $
        PW.withMainLoop $ \mainLoop -> do
            loop <- Raw.pw_main_loop_get_loop mainLoop
            PW.withContext loop \context ->
                PW.withCore context (flip go mainLoop)
  where
    go core mainLoop = do
        registry <- Raw.pw_core_get_registry core
        Raw.pw_with_spa_hook \registryListener -> do
            PW.withRegistryEvents handler removeHandler \registryEvent -> do
                Raw.pw_registry_add_listener registry registryListener registryEvent
                roundtrip core mainLoop
                putStrLn "Done!"

    roundtrip core mainLoop = do
        pendingRef <- newIORef (SeqID 0)
        PwCore.with_pw_core_events infoHandler (doneHandler mainLoop pendingRef) errorHandler \coreEvents -> do
            Raw.pw_with_spa_hook \coreListener -> do
                PwCore.pw_core_add_listener core coreListener coreEvents
                writeIORef pendingRef =<< PwCore.pw_core_sync core PwCore.pw_id_core
                print =<< Raw.pw_main_loop_run mainLoop

    doneHandler mainLoop pendingRef pwid seqid' = do
        pending <- readIORef pendingRef
        putStrLn $ "done: id:" <> show pwid <> " seq:" <> show seqid' <> " expected:" <> show pending
        when (pending == seqid') do
          putStrLn "Quitting"
          print =<< Raw.pw_main_loop_quit mainLoop

    infoHandler _coreInfo = pure ()

    errorHandler pwid seq' res msg = do
        putStrLn $ "error: id:" <> show pwid <> " seq:" <> show seq' <> " res:" <> show res <> " message:" <> show msg

    removeHandler pwid = do
        putStrLn $ "remove: " <> show pwid

    handler _pwid _typ _propsDict = pure ()
