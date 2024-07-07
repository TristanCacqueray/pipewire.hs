-- | Binding Objects - https://docs.pipewire.org/page_tutorial6.html
module Main (main) where

import Data.IORef
import Pipewire qualified as PW
import Pipewire.CoreAPI.Client qualified as PW

main :: IO ()
main = PW.withPipewire $ PW.withMainLoop $ \mainLoop -> do
    loop <- PW.pw_main_loop_get_loop mainLoop
    PW.withContext loop \context ->
        PW.withCore context (go mainLoop)
  where
    go mainLoop core = do
        registry <- PW.pw_core_get_registry core

        PW.withClientEvents \clientEvents -> do
            PW.withClientInfoHandler (clientHandler mainLoop) clientEvents do
                PW.withSpaHook \registryListener -> do
                    clientRef <- newIORef Nothing
                    PW.withRegistryEvents (handler registry clientRef clientEvents) removeHandler \registryEvent -> do
                        PW.pw_registry_add_listener registry registryListener registryEvent
                        PW.pw_main_loop_run mainLoop
                        readIORef clientRef >>= \case
                            Nothing -> pure ()
                            Just client -> PW.pw_proxy_destroy client.getProxy
                        putStrLn "Done!"

    removeHandler _ = pure ()

    clientHandler mainLoop pwid propsDict = do
        putStrLn $ "client: id:" <> show pwid
        putStrLn $ "props:"
        mapM_ (putStrLn . mappend "\t" . show) =<< PW.spaDictRead propsDict
        -- quit
        PW.pw_main_loop_quit mainLoop

    handler registry clientRef clientEvents pwid typ _version _props = do
        readIORef clientRef >>= \case
            Nothing -> case typ of
                "PipeWire:Interface:Client" -> do
                    client <- PW.bindClient registry pwid
                    PW.addClientListener client clientEvents
                    writeIORef clientRef (Just client)
                _ -> pure ()
            Just _client -> pure ()
