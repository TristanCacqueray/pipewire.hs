-- | A minimal pw-link implementation
module Main (main) where

import Data.Maybe (isJust)
import Data.Text (Text)
import System.Environment (getArgs)
import Text.Read (readMaybe)

import Pipewire qualified as PW
import Pipewire.IDMap (IDMap)
import Pipewire.IDMap qualified as IDMap

data Options = ListInputs | ListOutputs | ListLinks | DeleteLink PW.PwID | Connect PW.PwID PW.PwID

main :: IO ()
main =
    getArgs >>= \case
        ["list-inputs"] -> go ListInputs
        ["list-outputs"] -> go ListOutputs
        ["list-links"] -> go ListLinks
        ["disconnect", readMaybe -> Just pwid] -> go $ DeleteLink $ PW.PwID pwid
        ["connect", readMaybe -> Just out, readMaybe -> Just inp] -> go $ Connect (PW.PwID out) (PW.PwID inp)
        _ -> putStrLn "usage: hs-pw-link list-inputs|list-outputs|connect out in|disconnect pwid"
  where
    go options = PW.withInstance initialLinksRegistry updateLinksRegistry \pwInstance -> do
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
                        PW.syncState_ pwInstance \_ -> putStrLn "Link removed"
                    else putStrLn $ show pwid <> " is not a link!"
            Connect out inp -> do
                -- TODO: check out/inp are ports and that the links doesn't already exist

                -- Setup the link properties
                props <- PW.pw_properties_new
                PW.pw_properties_set_id props "link.output.port" out
                PW.pw_properties_set_id props "link.input.port" inp
                -- Keep the link after the program quit
                PW.pw_properties_set_linger props

                -- Create the link proxy
                pwLink <- PW.pw_link_create pwInstance.core props

                waitForLink pwLink pwInstance

                -- Cleanup the proxy
                -- That does not seem necessary as the pw_core_disconnect takes care of that,
                -- but that's what the pw-link.c is doing.
                PW.pw_proxy_destroy pwLink.getProxy

    waitForLink pwLink pwInstance = do
        let abort msg = putStrLn msg >> PW.quitInstance pwInstance
            destroyHandler = abort "Destroyed!"
            removedHandler = abort "Proxy Removed!"
            errorHandler res err = abort $ "error: " <> show res <> " " <> show err
        PW.with_pw_proxy_events pwLink.getProxy destroyHandler removedHandler errorHandler do
            let handler pwid state = case state of
                    Left err -> abort $ "Link state failed: " <> show err
                    Right PW.PW_LINK_STATE_ACTIVE -> do
                        putStrLn "Link is active, quiting the loop!"
                        PW.quitInstance pwInstance
                    Right x -> putStrLn $ show pwid <> ": " <> show x
            PW.with_pw_link_events pwLink handler do
                putStrLn "Waiting..."
                PW.runInstance pwInstance >>= \case
                    Just err -> print err
                    Nothing -> putStrLn "Done."

-- | For pw-link, we are only interested in the ports and the links
data LinksRegistry = LinksRegistry
    { outputs :: IDMap Text
    , inputs :: IDMap Text
    , links :: IDMap (PW.PwID, PW.PwID)
    }

initialLinksRegistry :: LinksRegistry
initialLinksRegistry = LinksRegistry mempty mempty mempty

-- | Update the 'LinksRegistry' on registry event.
updateLinksRegistry :: PW.RegistryEvent -> LinksRegistry -> IO LinksRegistry
updateLinksRegistry ev reg = case ev of
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
