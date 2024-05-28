-- | https://docs.pipewire.org/tutorial2_8c-example.html
module Main (main) where

import Control.Concurrent (forkIO, threadDelay)
import Pipewire qualified as PW

main :: IO ()
main =
    PW.withPipewire $
        PW.withMainLoop $ \mainLoop -> do
            loop <- PW.pw_main_loop_get_loop mainLoop
            PW.withContext loop \context ->
                PW.withCore context (go mainLoop)
  where
    go mainLoop core = do
        registry <- PW.pw_core_get_registry core

        -- _stopAfterTimeout mainLoop
        PW.with_spa_hook \registryListener -> do
            PW.with_pw_registry_events handler removeHandler \registryEvent -> do
                PW.pw_registry_add_listener registry registryListener registryEvent
                print =<< PW.pw_main_loop_run mainLoop
                putStrLn "Done!"

    removeHandler pwid = do
        putStrLn $ "remove: " <> show pwid

    handler pwid typ version propsDict = do
        props <- PW.spaDictRead propsDict
        putStrLn $ "object: id:" <> show pwid <> " type:" <> show typ <> " version:" <> show version
        mapM_ print props

    _stopAfterTimeout mainLoop =
        print =<< forkIO do
            putStrLn "Waiting 2sec before stopping the loop"
            threadDelay 2_000_000
            putStrLn "Quitting the loop"
            print =<< PW.pw_main_loop_quit mainLoop
