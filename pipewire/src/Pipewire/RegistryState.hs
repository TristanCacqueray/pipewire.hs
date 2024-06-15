-- | A high level API for managing links
module Pipewire.RegistryState where

import Data.Text (Text)
import Data.Text qualified as Text

import Data.List.NonEmpty (NonEmpty)
import Pipewire qualified as PW
import Pipewire.IDMap (IDMap)
import Pipewire.IDMap qualified as IDMap

withLinkInstance :: (PW.PwInstance RegistryState -> IO a) -> IO a
withLinkInstance = PW.withInstance initialRegistryState (const updateRegistryState)

-- | The registry state, to be maintained with the 'RegistryEvent'
data RegistryState = RegistryState
    { outputs :: IDMap Port
    , inputs :: IDMap Port
    , links :: IDMap (PW.PwID, PW.PwID)
    , nodes :: IDMap Node
    , devices :: IDMap Device
    }

initialRegistryState :: RegistryState
initialRegistryState = RegistryState mempty mempty mempty mempty mempty

findNode :: Text -> RegistryState -> Maybe (PW.PwID, Node)
findNode name reg = IDMap.find (\node -> name == node.name) reg.nodes

getNodeOutputs :: PW.PwID -> RegistryState -> [(PW.PwID, Port)]
getNodeOutputs pwid reg = IDMap.keep (\port -> pwid == port.nodeID) reg.outputs

getNodeInputs :: PW.PwID -> RegistryState -> [(PW.PwID, Port)]
getNodeInputs pwid reg = IDMap.keep (\port -> pwid == port.nodeID) reg.inputs

getLinkFrom :: PW.PwID -> RegistryState -> [(PW.PwID, PW.PwID)]
getLinkFrom pwid reg =
    map (\(linkid, (_out, inp)) -> (linkid, inp)) $
        IDMap.keep (\(out, _inp) -> pwid == out) reg.links

data Port = Port
    { alias :: Text
    , nodeID :: PW.PwID
    , kind :: PortKind
    }
    deriving (Show)

data AudioKind = Mono | FL | FR deriving (Show, Eq, Ord)

data PortKind = Audio (Maybe AudioKind) | Midi deriving (Show, Eq, Ord)

pattern AudioMono :: PortKind
pattern AudioMono = Audio (Just Mono)
pattern AudioFL :: PortKind
pattern AudioFL = Audio (Just FL)
pattern AudioFR :: PortKind
pattern AudioFR = Audio (Just FR)

getPortKind :: PW.SpaDict -> IO (Maybe PortKind)
getPortKind dict = do
    PW.spaDictLookup dict "audio.channel" >>= \case
        Just chan -> pure $ case chan of
            "FL" -> Just (Audio $ Just FL)
            "FR" -> Just (Audio $ Just FR)
            "MONO" -> Just (Audio $ Just Mono)
            _ -> Nothing
        Nothing ->
            PW.spaDictLookup dict "format.dsp" >>= \case
                Just fmt
                    | " midi" `Text.isSuffixOf` fmt -> pure (Just Midi)
                    | " audio" `Text.isSuffixOf` fmt -> pure (Just (Audio Nothing))
                _ -> pure Nothing

data Node = Node
    { name :: Text
    , deviceID :: Maybe PW.PwID
    }
    deriving (Show, Eq, Ord)

newtype Device = Device {name :: Text}

-- | Update the 'RegistryState' on registry event.
updateRegistryState :: PW.RegistryEvent -> RegistryState -> IO RegistryState
updateRegistryState ev reg = case ev of
    PW.Added pwid "PipeWire:Interface:Port" dict -> do
        newPort <-
            (,,,)
                <$> PW.spaDictLookup dict "port.alias"
                <*> PW.spaDictLookupInt dict "node.id"
                <*> PW.spaDictLookup dict "port.direction"
                <*> getPortKind dict
        -- putStrLn $ "Adding a new port: " <> show newPort
        let insert alias (PW.PwID -> nodeID) kind = IDMap.insert pwid (Port{alias, nodeID, kind})
        case newPort of
            (Just name, Just node, Just "in", Just kind) -> pure $ reg{inputs = insert name node kind reg.inputs}
            (Just name, Just node, Just "out", Just kind) -> pure $ reg{outputs = insert name node kind reg.outputs}
            _ -> putStrLn ("Unknown port: " <> show newPort) >> pure reg
    PW.Added pwid "PipeWire:Interface:Link" dict -> do
        newLink <- (,) <$> PW.spaDictLookupInt dict "link.output.port" <*> PW.spaDictLookupInt dict "link.input.port"
        case newLink of
            (Just out, Just inp) -> pure $ reg{links = IDMap.insert pwid (PW.PwID out, PW.PwID inp) reg.links}
            _ -> putStrLn ("Unknown link: " <> show newLink) >> pure reg
    PW.Added pwid "PipeWire:Interface:Node" dict -> do
        newNode <-
            (,)
                <$> PW.spaDictLookup dict "node.name"
                <*> (fmap PW.PwID <$> PW.spaDictLookupInt dict "device.id")
        case newNode of
            (Just name, deviceID) -> pure $ reg{nodes = IDMap.insert pwid (Node{name, deviceID}) reg.nodes}
            _ -> putStrLn "Unknown node" >> pure reg
    PW.Added pwid "PipeWire:Interface:Device" dict -> do
        PW.spaDictLookup dict "device.nick" >>= \case
            Just name -> pure $ reg{devices = IDMap.insert pwid (Device name) reg.devices}
            Nothing -> putStrLn "Unknown device" >> pure reg
    PW.Added{} -> pure reg
    PW.Removed pwid ->
        pure $
            RegistryState
                { inputs = IDMap.delete pwid reg.inputs
                , outputs = IDMap.delete pwid reg.outputs
                , links = IDMap.delete pwid reg.links
                , nodes = IDMap.delete pwid reg.nodes
                , devices = IDMap.delete pwid reg.devices
                }

data LinkResult
    = LinkFailed (NonEmpty PW.CoreError)
    | LinkMissmatched
    | LinkSuccess
    deriving (Show)

getLinkablePorts :: PW.PwID -> PW.PwID -> RegistryState -> [(PW.PwID, PW.PwID)]
getLinkablePorts source sink reg =
    case (getNodeOutputs source reg, getNodeInputs sink reg) of
        ([(out, _)], [(inp, _)]) -> [(out, inp)]
        _ -> []

linkNodes :: PW.PwID -> PW.PwID -> PW.PwInstance RegistryState -> IO LinkResult
linkNodes source sink pwInstance =
    PW.syncState pwInstance >>= \case
        Left err -> pure $ LinkFailed err
        Right reg -> case (getNodeOutputs source reg, getNodeInputs sink reg) of
            ([(out, _)], [(inp, _)]) -> PW.withLink pwInstance.core (PW.LinkProperties out inp False) \pwLink -> do
                PW.waitForLink pwLink pwInstance >>= \case
                    Just err -> pure $ LinkFailed err
                    Nothing -> pure LinkSuccess
            _ -> pure LinkMissmatched
