-- | https://docs.pipewire.org/tutorial2_8c-example.html
module Main (main) where

import Control.Concurrent (forkIO, threadDelay)
import Pipewire qualified as PW
import Pipewire.Raw qualified as Raw

main :: IO ()
main =
    PW.withPipewire $
        PW.withMainLoop $ \mainLoop -> do
            loop <- Raw.pw_main_loop_get_loop mainLoop
            PW.withContext loop \context ->
                PW.withCore context (go mainLoop)
  where
    go mainLoop core = do
        registry <- Raw.pw_core_get_registry core

        -- _stopAfterTimeout mainLoop
        Raw.pw_with_spa_hook \registryListener -> do
            PW.withRegistryEvents handler removeHandler \registryEvent -> do
                Raw.pw_registry_add_listener registry registryListener registryEvent
                print =<< Raw.pw_main_loop_run mainLoop
                putStrLn "Done!"

    removeHandler pwid = do
        putStrLn $ "remove: " <> show pwid

    handler pwid typ propsDict = do
        props <- Raw.spaDictRead propsDict
        putStrLn $ "object: id:" <> show pwid <> " type:" <> show typ
        mapM_ print props

    _stopAfterTimeout mainLoop =
        print =<< forkIO do
            putStrLn "Waiting 2sec before stopping the loop"
            threadDelay 2_000_000
            putStrLn "Quitting the loop"
            print =<< Raw.pw_main_loop_quit mainLoop
