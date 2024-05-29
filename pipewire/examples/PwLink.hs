-- | A minimal hs-pw-link implementation
module Main (main) where

import Data.Maybe (isJust)
import Data.Text (Text)
import System.Environment (getArgs)
import Text.Read (readMaybe)

import Pipewire qualified as PW
import Pipewire.IDMap (IDMap)
import Pipewire.IDMap qualified as IDMap

data Options = ListInputs | ListOutputs | ListLinks | DeleteLink PW.PwID

main :: IO ()
main =
    getArgs >>= \case
        ["list-inputs"] -> go ListInputs
        ["list-outputs"] -> go ListOutputs
        ["list-links"] -> go ListLinks
        ["disconnect", (readMaybe -> Just pwid)] -> go $ DeleteLink $ PW.PwID pwid
        _ -> putStrLn "usage: hs-pw-link list-inputs|list-outputs|connect out in|disconnect pwid"
  where
    go options = PW.withInstance initialSimpleRegistry updateSimpleRegistry \pwInstance -> do
        case options of
            ListInputs -> do
                PW.syncState_ pwInstance \simpleRegistry ->
                    mapM_ print $ IDMap.toList simpleRegistry.inputs
            ListOutputs -> do
                PW.syncState_ pwInstance \simpleRegistry ->
                    mapM_ print $ IDMap.toList simpleRegistry.outputs
            ListLinks -> do
                PW.syncState_ pwInstance \simpleRegistry ->
                    mapM_ print $ IDMap.toList simpleRegistry.links
            DeleteLink pwid -> do
                isLink <- PW.syncState_ pwInstance \simpleRegistry ->
                    pure . isJust $ IDMap.lookup pwid simpleRegistry.links
                if isLink
                    then do
                        PW.pw_registry_destroy pwInstance.registry pwid
                        PW.syncState_ pwInstance \_ -> pure ()
                    else putStrLn $ show pwid <> " is not a link!"

data SimpleRegistry = SimpleRegistry
    { outputs :: IDMap Text
    , inputs :: IDMap Text
    , links :: IDMap (PW.PwID, PW.PwID)
    }

initialSimpleRegistry :: SimpleRegistry
initialSimpleRegistry = SimpleRegistry mempty mempty mempty

updateSimpleRegistry :: PW.RegistryEvent -> SimpleRegistry -> IO SimpleRegistry
updateSimpleRegistry ev reg = case ev of
    PW.Added pwid "PipeWire:Interface:Port" dict -> do
        newPort <-
            (,)
                <$> PW.spaDictLookup dict "port.alias"
                <*> PW.spaDictLookup dict "port.direction"
        case newPort of
            (Just name, Just "in") -> pure $ reg{inputs = IDMap.insert pwid name reg.inputs}
            (Just name, Just "out") -> pure $ reg{outputs = IDMap.insert pwid name reg.outputs}
            _ -> putStrLn ("Unknown port: " <> show newPort) >> pure reg
    PW.Added pwid "PipeWire:Interface:Link" dict -> do
        newLink <- (,) <$> PW.spaDictLookupInt dict "link.output.port" <*> PW.spaDictLookupInt dict "link.input.port"
        case newLink of
            (Just out, Just inp) -> pure $ reg{links = IDMap.insert pwid (PW.PwID out, PW.PwID inp) reg.links}
            _ -> putStrLn ("Unknown link: " <> show newLink) >> pure reg
    PW.Added{} -> pure reg
    PW.Removed pwid ->
        pure $
            reg
                { inputs = IDMap.delete pwid reg.inputs
                , outputs = IDMap.delete pwid reg.outputs
                , links = IDMap.delete pwid reg.links
                }
