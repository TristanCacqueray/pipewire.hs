-- | A minimal pw-link implementation
module Main (main) where

import Data.Maybe (isJust)
import Data.Text (Text, pack)
import System.Environment (getArgs)
import Text.Read (readMaybe)

import Pipewire qualified as PW
import Pipewire.IDMap qualified as IDMap
import Pipewire.RegistryState qualified as PW

data ListKind = Inputs | Outputs | Links | Nodes

data Options = List ListKind | DeleteLink PW.PwID | Connect PW.PwID PW.PwID | LinkNode Text Text

main :: IO ()
main =
    getArgs >>= \case
        ["list", "inputs"] -> go $ List Inputs
        ["list", "outputs"] -> go $ List Outputs
        ["list", "links"] -> go $ List Links
        ["list", "nodes"] -> go $ List Nodes
        ["disconnect", readMaybe -> Just pwid] -> go $ DeleteLink $ PW.PwID pwid
        ["connect", readMaybe -> Just out, readMaybe -> Just inp] -> go $ Connect (PW.PwID out) (PW.PwID inp)
        ["link-node", pack -> source, pack -> sink] -> go $ LinkNode source sink
        _ -> putStrLn "usage: hs-pw-link list (inputs|outputs|links|nodes)|connect out in|disconnect pwid|link-node out in"
  where
    go options = PW.withLinkInstance \pwInstance -> do
        case options of
            List listKind -> do
                PW.syncState_ pwInstance >>= \simpleRegistry ->
                    case listKind of
                        Inputs -> mapM_ print $ IDMap.toList simpleRegistry.inputs
                        Outputs -> mapM_ print $ IDMap.toList simpleRegistry.outputs
                        Links -> mapM_ print $ IDMap.toList simpleRegistry.links
                        Nodes -> mapM_ print $ IDMap.toList simpleRegistry.nodes
            DeleteLink pwid -> do
                isLink <-
                    PW.syncState_ pwInstance >>= \simpleRegistry ->
                        pure . isJust $ IDMap.lookup pwid simpleRegistry.links
                if isLink
                    then do
                        PW.pw_registry_destroy pwInstance.registry pwid
                        PW.syncState_ pwInstance >> putStrLn "Link removed"
                    else putStrLn $ show pwid <> " is not a link!"
            Connect out inp -> do
                -- TODO: check out/inp are ports and that the links doesn't already exist
                PW.withLink pwInstance.core (PW.LinkProperties out inp True) \pwLink -> do
                    PW.waitForLink pwLink pwInstance >>= \case
                        Just err -> print err
                        Nothing -> putStrLn "Done."
            LinkNode source sink -> do
                (sourceID, sinkID) <-
                    PW.syncState_ pwInstance >>= \reg ->
                        case (PW.findNode source reg, PW.findNode sink reg) of
                            (Just oid, Just iid) -> pure (fst oid, fst iid)
                            res -> error $ "Could not find: " <> show res
                print =<< PW.linkNodes sourceID sinkID pwInstance
                putStrLn "Exit to disconnect..."
                print =<< PW.runInstance pwInstance
