-- | A high level API for managing links
module Pipewire.Instance where

import Data.Text (Text)

import Data.List.NonEmpty (NonEmpty)
import Pipewire qualified as PW
import Pipewire.IDMap (IDMap)
import Pipewire.IDMap qualified as IDMap

withLinkInstance :: (PW.PwInstance LinksRegistry -> IO a) -> IO a
withLinkInstance = PW.withInstance initialLinksRegistry (const updateLinksRegistry)

-- | For pw-link, we are only interested in the ports and the links
data LinksRegistry = LinksRegistry
    { outputs :: IDMap Port
    , inputs :: IDMap Port
    , links :: IDMap (PW.PwID, PW.PwID)
    , nodes :: IDMap Node
    }

findNode :: Text -> LinksRegistry -> Maybe (PW.PwID, Node)
findNode name reg = IDMap.find (\(Node nodeName) -> name == nodeName) reg.nodes

getNodeOutputs :: PW.PwID -> LinksRegistry -> [(PW.PwID, Port)]
getNodeOutputs pwid reg = IDMap.keep (\(Port _ nodeID) -> pwid == nodeID) reg.outputs

getNodeInputs :: PW.PwID -> LinksRegistry -> [(PW.PwID, Port)]
getNodeInputs pwid reg = IDMap.keep (\(Port _ nodeID) -> pwid == nodeID) reg.inputs

getLinkFrom :: PW.PwID -> LinksRegistry -> [(PW.PwID, PW.PwID)]
getLinkFrom pwid reg =
    map (\(linkid, (_out, inp)) -> (linkid, inp)) $
        IDMap.keep (\(out, _inp) -> pwid == out) reg.links

data Port = Port
    { alias :: Text
    , nodeID :: PW.PwID
    }
    deriving (Show)

newtype Node = Node Text
    deriving newtype (Show, Eq, Ord)

initialLinksRegistry :: LinksRegistry
initialLinksRegistry = LinksRegistry mempty mempty mempty mempty

-- | Update the 'LinksRegistry' on registry event.
updateLinksRegistry :: PW.RegistryEvent -> LinksRegistry -> IO LinksRegistry
updateLinksRegistry ev reg = case ev of
    PW.Added pwid "PipeWire:Interface:Port" dict -> do
        newPort <-
            (,,)
                <$> PW.spaDictLookup dict "port.alias"
                <*> PW.spaDictLookupInt dict "node.id"
                <*> PW.spaDictLookup dict "port.direction"
        -- putStrLn $ "Adding a new port: " <> show newPort
        let insert name node = IDMap.insert pwid (Port name (PW.PwID node))
        case newPort of
            (Just name, Just node, Just "in") -> pure $ reg{inputs = insert name node reg.inputs}
            (Just name, Just node, Just "out") -> pure $ reg{outputs = insert name node reg.outputs}
            _ -> putStrLn ("Unknown port: " <> show newPort) >> pure reg
    PW.Added pwid "PipeWire:Interface:Link" dict -> do
        newLink <- (,) <$> PW.spaDictLookupInt dict "link.output.port" <*> PW.spaDictLookupInt dict "link.input.port"
        case newLink of
            (Just out, Just inp) -> pure $ reg{links = IDMap.insert pwid (PW.PwID out, PW.PwID inp) reg.links}
            _ -> putStrLn ("Unknown link: " <> show newLink) >> pure reg
    PW.Added pwid "PipeWire:Interface:Node" dict -> do
        PW.spaDictLookup dict "node.name" >>= \case
            Just name -> pure $ reg{nodes = IDMap.insert pwid (Node name) reg.nodes}
            Nothing -> putStrLn "Unknown node" >> pure reg
    PW.Added{} -> pure reg
    PW.Removed pwid ->
        pure $
            reg
                { inputs = IDMap.delete pwid reg.inputs
                , outputs = IDMap.delete pwid reg.outputs
                , links = IDMap.delete pwid reg.links
                }

data LinkResult
    = LinkFailed (NonEmpty PW.CoreError)
    | LinkMissmatched
    | LinkSuccess
    deriving (Show)

getLinkablePorts :: PW.PwID -> PW.PwID -> LinksRegistry -> [(PW.PwID, PW.PwID)]
getLinkablePorts source sink reg =
    case (getNodeOutputs source reg, getNodeInputs sink reg) of
        ([(out, _)], [(inp, _)]) -> [(out, inp)]
        _ -> []

linkNodes :: PW.PwID -> PW.PwID -> PW.PwInstance LinksRegistry -> IO LinkResult
linkNodes source sink pwInstance =
    PW.syncState pwInstance \case
        Left err -> pure $ LinkFailed err
        Right reg -> case (getNodeOutputs source reg, getNodeInputs sink reg) of
            ([(out, _)], [(inp, _)]) -> PW.withLink pwInstance.core (PW.LinkProperties out inp False) \pwLink -> do
                PW.waitForLink pwLink pwInstance >>= \case
                    Just err -> pure $ LinkFailed err
                    Nothing -> pure LinkSuccess
            _ -> pure LinkMissmatched
