module Main (main) where

import Data.List (find)
import Data.Text (pack)
import Pipewire qualified as PW
import RIO
import System.Environment (getArgs)

data Metadata = Metadata
    { pwid :: PW.PwID
    , name :: Text
    }
    deriving (Show)

main :: IO ()
main =
    getArgs >>= \case
        ["list"] -> withMetadatas \_ metadatas -> mapM_ print metadatas
        ["monitor"] -> withMetadatas \pwInstance _ -> print =<< PW.runInstance pwInstance
        ["list", pack -> name] ->
            withMetadataName name \pwInstance metadata -> PW.withMetadataListener propertyHandler metadata do
                sync pwInstance
        ["set", readMaybe -> Just optID, pack -> key, pack -> value] ->
            withMetadataName "default" \pwInstance metadata -> do
                putStrLn $ "Setting " <> show (optID, key, value)
                PW.metadataSetProperty metadata optID key value
                sync pwInstance
        ["delete"] ->
            withMetadataName "default" \pwInstance metadata -> do
                putStrLn "Clearing metadata"
                PW.pw_metadata_clear metadata
                sync pwInstance
        ["delete", readMaybe -> Just optID, pack -> key] ->
            withMetadataName "default" \pwInstance metadata -> do
                putStrLn $ "Deleteing " <> show (optID, key)
                PW.metadataDeleteProperty metadata optID key
                sync pwInstance
        _ -> putStrLn "usage: hs-pw-metadata list|monitor|list name|set id key value|delete|delete id key"

-- process the events
sync :: PW.PwInstance state -> IO ()
sync pwInstance = PW.syncState_ pwInstance >> putStrLn "done."

propertyHandler :: PW.PwID -> PW.MetadataProperty -> IO Int
propertyHandler pwid prop = do
    putStrLn $ "update: " <> show pwid <> ", prop: " <> show prop
    pure 0

withMetadataName :: Text -> (PW.PwInstance [Metadata] -> PW.Metadata -> IO ()) -> IO ()
withMetadataName name cb = withMetadatas \pwInstance metadatas -> do
    case find (\m -> m.name == name) metadatas of
        Nothing -> putStrLn "Not found!"
        Just Metadata{pwid} -> PW.withMetadata pwInstance.registry pwid (cb pwInstance)

withMetadatas :: (PW.PwInstance [Metadata] -> [Metadata] -> IO a) -> IO a
withMetadatas cb = do
    PW.withInstance [] \_ pwInstance -> do
        PW.withRegistryHandler pwInstance (handleEvent pwInstance.stateVar) do
            cb pwInstance =<< PW.syncState_ pwInstance
  where
    handleEvent stateV ev = do
        case ev of
            PW.Added _ pwid "PipeWire:Interface:Metadata" props -> do
                PW.spaDictLookup props "metadata.name" >>= \case
                    Nothing -> putStrLn $ show pwid <> ": metadata with name?!"
                    Just name -> modifyMVar_ stateV (\xs -> pure (Metadata pwid name : xs))
            _ -> pure ()
