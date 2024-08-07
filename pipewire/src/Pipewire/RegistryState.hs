-- | A high level API for managing links
module Pipewire.RegistryState where

import Data.Text (Text)
import Data.Text qualified as Text

import Control.Concurrent (modifyMVar_)
import Data.List.NonEmpty (NonEmpty)
import Pipewire qualified as PW
import Pipewire.IDMap (IDMap)
import Pipewire.IDMap qualified as IDMap

withInstanceRegistryState :: (PW.PwInstance RegistryState -> RegistryState -> IO a) -> IO a
withInstanceRegistryState cb = PW.withInstance initialRegistryState \_ pwInstance -> do
    let updateState ev = modifyMVar_ pwInstance.stateVar (updateRegistryState ev)
    PW.withRegistryHandler pwInstance updateState do
        state <- PW.syncState_ pwInstance
        cb pwInstance state

-- | The registry state, to be maintained with the 'RegistryEvent'
data RegistryState = RegistryState
    { outputs :: IDMap Port
    , inputs :: IDMap Port
    , links :: IDMap (PW.PwID, PW.PwID)
    , nodes :: IDMap Node
    , devices :: IDMap Device
    }

displayRegistryState :: RegistryState -> Text
displayRegistryState state =
    Text.unlines
        [ "devices:"
        , displayMap state.devices
        , "nodes:"
        , displayMap state.nodes
        , "outputs:"
        , displayMap state.outputs
        , "inputs:"
        , displayMap state.inputs
        , "links:" <> Text.unwords (map (Text.pack . show) $ IDMap.toList state.links)
        ]
  where
    displayMap :: (Show a) => IDMap a -> Text
    displayMap idm = Text.unlines $ map (mappend " - " . Text.pack . show) $ IDMap.toList idm

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

data PortKind = Audio (Maybe AudioKind) | Midi | Video deriving (Show, Eq, Ord)

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
                _ ->
                    -- TODO: better detect video ports
                    pure (Just Video)

data Node = Node
    { name :: Text
    , deviceID :: Maybe PW.PwID
    , media :: Maybe Text
    }
    deriving (Show, Eq, Ord)

newtype Device = Device {name :: Text} deriving (Show)

-- | Update the 'RegistryState' on registry event.
updateRegistryState :: PW.RegistryEvent -> RegistryState -> IO RegistryState
updateRegistryState ev reg = case ev of
    PW.Added _ pwid "PipeWire:Interface:Port" dict -> do
        newPort <-
            (,,,)
                <$> PW.spaDictLookup dict "port.alias"
                <*> PW.spaDictLookupInt dict "node.id"
                <*> PW.spaDictLookup dict "port.direction"
                <*> getPortKind dict
        -- putStrLn $ "Adding a new port: " <> show newPort
        let insert alias (PW.PwID -> nodeID) kind = IDMap.insert pwid (Port{alias, nodeID, kind})
        case newPort of
            (_, _, _, Nothing) -> do
                putStrLn $ "Could not determine the port kind for " <> show pwid
                mapM_ print =<< PW.spaDictRead dict
                pure reg
            (Just name, Just node, Just "in", Just kind) -> pure $ reg{inputs = insert name node kind reg.inputs}
            (Just name, Just node, Just "out", Just kind) -> pure $ reg{outputs = insert name node kind reg.outputs}
            _ -> putStrLn ("Unknown port: " <> show newPort) >> pure reg
    PW.Added _ pwid "PipeWire:Interface:Link" dict -> do
        newLink <- (,) <$> PW.spaDictLookupInt dict "link.output.port" <*> PW.spaDictLookupInt dict "link.input.port"
        case newLink of
            (Just out, Just inp) -> pure $ reg{links = IDMap.insert pwid (PW.PwID out, PW.PwID inp) reg.links}
            _ -> putStrLn ("Unknown link: " <> show newLink) >> pure reg
    PW.Added _ pwid "PipeWire:Interface:Node" dict -> handleNode pwid dict
    PW.Added _ pwid "PipeWire:Interface:Device" dict -> do
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
  where
    handleNode pwid dict = do
        newNode <-
            (,)
                <$> PW.spaDictLookup dict "node.name"
                <*> (fmap PW.PwID <$> PW.spaDictLookupInt dict "device.id")
        media <- PW.spaDictLookup dict "media.name"
        case newNode of
            (Just name, deviceID) -> pure $ reg{nodes = IDMap.insert pwid (Node{name, deviceID, media}) reg.nodes}
            _ -> putStrLn "Unknown node" >> pure reg

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

displayWithPwID :: PW.PwID -> Maybe Text -> Text
displayWithPwID pwid mText = case mText of
    Nothing -> pwid'
    Just text -> mconcat [pwid', ":", text]
  where
    pwid' = Text.pack (show pwid)

displayPort :: IDMap Port -> PW.PwID -> Text
displayPort ports pwid = displayWithPwID pwid $ fmap (.alias) $ IDMap.lookup pwid ports

displayLinkPorts :: RegistryState -> (PW.PwID, PW.PwID) -> Text
displayLinkPorts state (out, inp) = Text.unwords [displayPort state.outputs out, "->", displayPort state.inputs inp]

displayLink :: RegistryState -> PW.PwID -> Text
displayLink state pwid = displayWithPwID pwid $ fmap (displayLinkPorts state) $ IDMap.lookup pwid state.links
