-- | https://docs.pipewire.org/tutorial3_8c-example.html
module Main (main) where

import Control.Monad (when)
import Data.IORef

import Pipewire qualified as PW

main :: IO ()
main =
    PW.withPipewire $
        PW.withMainLoop $ \mainLoop -> do
            loop <- PW.pw_main_loop_get_loop mainLoop
            PW.withContext loop \context ->
                PW.withCore context (flip go mainLoop)
  where
    go core mainLoop = do
        registry <- PW.pw_core_get_registry core
        PW.with_spa_hook \registryListener -> do
            PW.with_pw_registry_events handler removeHandler \registryEvent -> do
                PW.pw_registry_add_listener registry registryListener registryEvent
                roundtrip core mainLoop
                putStrLn "Done!"

    roundtrip core mainLoop = do
        pendingRef <- newIORef (PW.SeqID 0)
        PW.with_pw_core_events infoHandler (doneHandler mainLoop pendingRef) errorHandler \coreEvents -> do
            PW.with_spa_hook \coreListener -> do
                PW.pw_core_add_listener core coreListener coreEvents
                writeIORef pendingRef =<< PW.pw_core_sync core PW.pw_id_core
                print =<< PW.pw_main_loop_run mainLoop

    doneHandler mainLoop pendingRef pwid seqid' = do
        pending <- readIORef pendingRef
        putStrLn $ "done: id:" <> show pwid <> " seq:" <> show seqid' <> " expected:" <> show pending
        when (pending == seqid') do
            putStrLn "Quitting"
            print =<< PW.pw_main_loop_quit mainLoop

    infoHandler _coreInfo = pure ()

    errorHandler pwid seq' res msg = do
        putStrLn $ "error: id:" <> show pwid <> " seq:" <> show seq' <> " res:" <> show res <> " message:" <> show msg

    removeHandler pwid = do
        putStrLn $ "remove: " <> show pwid

    handler _pwid _typ _version _propsDict = pure ()
