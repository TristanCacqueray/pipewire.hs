-- | The program entry point
module Main (main) where

import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Options.Applicative qualified as O
import RIO
import RIO.FilePath ((</>))
import System.Environment (getEnv)

import Pipewire qualified as PW
import Pipewire.RegistryState

import PwController.Eval
import PwController.Expr
import PwController.Parser

doApplyConfig :: Usage Rules -> RegistryState -> UpdateChan -> IO ()
doApplyConfig usage state updateChan = do
    putStrLn "\n[*] Applying config..."
    let actions = applyConfig state usage.config
    when (null actions) do
        putStrLn "No action needed"
    forM_ actions \case
        AddLink (out, inp) -> do
            Text.putStrLn $ "Adding link: " <> displayLinkPorts state (out, inp)
            unless usage.dry do
                let linger = True
                PW.withLock updateChan.threadLoop do
                    PW.withLink updateChan.pwInstance.core (PW.LinkProperties out inp linger) \pwLink ->
                        PW.waitForLinkAsync pwLink updateChan.threadLoop >>= \case
                            Nothing -> pure ()
                            Just (Left err) -> putStrLn $ "Bad link: " <> show err
                            Just (Right err) -> putStrLn $ "Link failed: " <> show err
                putStrLn "add link completed"
        DelLink pwid -> do
            Text.putStrLn $ "Removing link: " <> displayLink state pwid
            unless usage.dry do
                PW.withLock updateChan.threadLoop do
                    PW.pw_registry_destroy updateChan.pwInstance.registry pwid
                putStrLn "del link completed"
        Err e -> putStrLn ("Error: " <> show e)

-- | Monitoring thread
data UpdateChan = UpdateChan
    { pwInstance :: PW.PwInstance RegistryState
    , threadLoop :: PW.ThreadLoop
    , state :: IORef RegistryState
    , baton :: MVar ()
    }

controllerLoop :: Usage Rules -> UpdateChan -> IO ()
controllerLoop usage updateChan = go
  where
    go = do
        () <- takeMVar updateChan.baton
        threadDelay 200_000
        isEmptyMVar updateChan.baton >>= \case
            False -> go
            -- Wait for at least 200ms of no update before handling the rules
            True -> handleUpdate
    handleUpdate = do
        state <- readIORef updateChan.state
        doApplyConfig usage state updateChan
        go

withControllerInstance :: (UpdateChan -> RegistryState -> IO a) -> IO a
withControllerInstance cb = PW.withThreadedInstance initialRegistryState \threadLoop pwInstance -> do
    syncCompleted <- newIORef False
    updateChan <- UpdateChan pwInstance threadLoop <$> newIORef initialRegistryState <*> newEmptyMVar
    let updateState ev = do
            state <- modifyMVar pwInstance.stateVar \prevState -> do
                newState <- updateRegistryState ev prevState
                pure (newState, newState)
            readIORef syncCompleted >>= \case
                True -> do
                    writeIORef updateChan.state state
                    void $ tryPutMVar updateChan.baton ()
                _ -> pure ()
    PW.withRegistryHandler pwInstance updateState do
        state <- PW.syncState_ pwInstance
        writeIORef syncCompleted True
        cb updateChan state

mainLoop :: Usage Rules -> IO ()
mainLoop usage = withControllerInstance \updateChan state -> do
    when usage.debug do
        printDebug state usage.config

    -- apply rules on the initial config
    doApplyConfig usage state updateChan

    unless usage.oneShot do
        race_ (controllerLoop usage updateChan) do
            putStrLn "Running the main loop"
            print =<< PW.runInstance updateChan.pwInstance

printDebug :: RegistryState -> Rules -> IO ()
printDebug state config = do
    Text.putStrLn "\n== initial state =="
    Text.putStrLn $ displayRegistryState state
    Text.putStrLn "\n== config matcher =="
    forM_ (debugTarget state config) \(target, res) -> do
        Text.putStr $ Text.unwords [Text.pack (show target.kind), Text.pack (show target.matcher)]
        case res of
            Left err -> putStrLn $ "  " <> show err
            Right xs -> putStrLn ":" >> mapM_ (putStrLn . mappend " " . show) xs

-- | Command Line Interface
data Usage a = Usage
    { debug :: Bool
    , dry :: Bool
    , oneShot :: Bool
    , config :: a
    }

usageParser :: O.Parser (Usage (Maybe FilePath))
usageParser =
    Usage
        <$> O.switch (O.long "debug" <> O.help "Debug internal state")
        <*> O.switch (O.long "dry" <> O.help "Do not apply rules, just show the pw-link commands")
        <*> O.switch (O.long "one-shot" <> O.help "Do not monitor, just apply rules to the current state")
        <*> optional (O.strOption (O.long "config" <> O.metavar "PATH" <> O.help "Config path, default to ~/.config/pw-controller.conf"))

getUsage :: IO (Usage Rules)
getUsage = do
    usage <- O.execParser opts
    fp <- case usage.config of
        Nothing -> do
            home <- getEnv "HOME"
            pure $ home </> ".config" </> "pw-controller.conf"
        Just fp -> pure fp
    parseConfig fp >>= \case
        Left err -> error err
        Right config -> pure $ usage{config}
  where
    opts =
        O.info
            (usageParser O.<**> O.helper)
            (O.fullDesc <> O.header "pw-controller - a pipewire controller")

main :: IO ()
main = do
    usage <- getUsage
    when usage.debug do
        mapM_ print usage.config
    mainLoop usage
