-- | The program entry point
module Main (main) where

import Data.Attoparsec.ByteString.Streaming qualified as AS
import Data.ByteString qualified as BS hiding (pack)
import Data.Text.IO qualified as Text
import Options.Applicative qualified as O
import RIO
import RIO.FilePath ((</>))
import RIO.Process qualified as P hiding (proc)
import Streaming.ByteString.Char8 qualified as Q
import System.Environment (getEnv)
import System.Process.Typed qualified as P (proc)

import PwConstraint
import PwMonParser
import PwState

data UpdateChan = UpdateChan
    { state :: IORef State
    , baton :: MVar ()
    }

eventsLoop :: Usage any -> UpdateChan -> Q.ByteStream IO () -> IO ()
eventsLoop usage updateChan = skipInit
  where
    skipInit bs = do
        (intro, rest) <- AS.parse introParser bs
        case intro of
            Right _ -> go rest
            Left err -> print ("Couldn't read the intro" :: Text, err)

    go bs = do
        (res, rest) <- AS.parse eventParser bs
        case res of
            Left err -> putStrLn $ "Event parser failed: " <> show err
            Right eev -> do
                case eev of
                    Left err
                        | usage.debug -> putStrLn $ "Unknown event: " <> show err
                        | otherwise -> pure ()
                    Right ev -> do
                        modifyIORef' updateChan.state (processEvent ev)
                        void $ tryPutMVar updateChan.baton ()
                go rest

controllerLoop :: Usage Constraint -> UpdateChan -> IO ()
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
        let actions = applyConstraints usage.config state
        when (usage.debug || usage.status) do
            printBS $ showState state
        when usage.status do
            putStrLn "config:"
            print usage.config
            case actions of
                [] -> pure ()
                xs -> putStrLn "actions:" >> mapM_ print xs
            exitSuccess
        forM_ actions \case
            AddLink link' -> do
                when (usage.debug || usage.dry) do
                    putStrLn $ "Adding link: " <> show link'
                unless usage.dry do addPwLink link'
            DelLink pwid -> do
                when (usage.debug || usage.dry) do
                    putStrLn $ "Removing link: " <> show pwid
                unless usage.dry do delPwLink pwid
        go

mainLoop :: Usage Constraint -> Q.ByteStream IO () -> IO ()
mainLoop usage bs = do
    updateChan <- UpdateChan <$> newIORef initialState <*> newEmptyMVar
    race_ (eventsLoop usage updateChan bs) (controllerLoop usage updateChan)

withPwMon :: (Q.ByteStream IO () -> IO ()) -> IO ()
withPwMon cb = P.withProcessWait procConfig \p -> do
    cb (Q.fromHandle (P.getStdout p))
  where
    procConfig = P.setStdout P.createPipe $ P.proc "pw-mon" ["--hide-params", "--print-separator"]

addPwLink :: (PwID, PwID) -> IO ()
addPwLink (PwID out, PwID inp) = runPwLink [show out, show inp]

delPwLink :: PwID -> IO ()
delPwLink (PwID pwid) = runPwLink ["-d", show pwid]

runPwLink :: [String] -> IO ()
runPwLink = P.runProcess_ . P.proc "pw-link"

data Usage a = Usage
    { debug :: Bool
    , dry :: Bool
    , status :: Bool
    , config :: a
    }

usageParser :: O.Parser (Usage (Maybe FilePath))
usageParser =
    Usage
        <$> O.switch (O.long "debug" <> O.help "Debug internal state")
        <*> O.switch (O.long "dry" <> O.help "Do not apply rules, just show the pw-link commands")
        <*> O.switch (O.long "status" <> O.help "Print the current state")
        <*> optional (O.strOption (O.long "config" <> O.metavar "PATH" <> O.help "Config path, default to ~/.config/pw-controller.conf"))

getUsage :: IO (Usage Constraint)
getUsage = do
    usage <- O.execParser opts
    fp <- case usage.config of
        Nothing -> do
            home <- getEnv "HOME"
            pure $ home </> ".config" </> "pw-controller.conf"
        Just fp -> pure fp
    content <- BS.readFile fp
    case parseConstraint content of
        Left e -> error $ fp <> ": " <> show e
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
        print usage.config
    forever do
        withPwMon (mainLoop usage)
        Text.putStrLn "Ooops, restarting..."
        threadDelay 1_000_000

printBS :: ByteString -> IO ()
printBS = Text.putStrLn . RIO.decodeUtf8Lenient
