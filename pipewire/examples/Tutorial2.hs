-- | https://docs.pipewire.org/tutorial2_8c-example.html
module Main (main) where

import Control.Concurrent (forkIO, threadDelay)
import Pipewire
import Pipewire.Raw qualified as Raw

main :: IO ()
main = do
    pwInit
    mainLoop <- Raw.pw_main_loop_new
    context <- Raw.pw_context_new =<< Raw.pw_main_loop_get_loop mainLoop
    core <- Raw.pw_context_connect context
    registry <- Raw.pw_core_get_registry core

    -- _stopAfterTimeout mainLoop
    Raw.pw_with_spa_hook \registryListener -> do
        pwWithRegistryEvents handler \registryEvent -> do
            Raw.pw_registry_add_listener registry registryListener registryEvent
            print =<< Raw.pw_main_loop_run mainLoop
            putStrLn "Done!"

    Raw.pw_core_disconnect core
    Raw.pw_context_destroy context
    Raw.pw_main_loop_destroy mainLoop
  where
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
