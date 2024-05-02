-- | This module is in charge of maintaining the State of pipewire
module PwState (
    State (..),
    initialState,
    processEvent,
    showState,
    -- * elems
    PwID (..),
    PwPath (..),
    PwEvent (..),
    Port (..),
    Link (..),
    Node (..),
    Device (..),
    PortKind (..),
)
where

import Data.ByteString.Char8 qualified as BS
import Data.IntMap.Strict qualified as IM
import RIO

import Data.List (intercalate)

-- | Global pipewire identifier
newtype PwID = PwID Int
    deriving newtype (Show, Ord, Eq, Num)

-- | Port are the end of a link
data Port = Port
    { nodeID :: PwID
    , kind :: PortKind
    , path :: PwPath
    }

instance Show Port where
    show p = mconcat [show p.nodeID, "|", show p.path]

data PortKind = AudioMono | AudioFL | AudioFR | Midi deriving (Show, Eq)

newtype Device = Device {name :: ByteString}
    deriving newtype (Show)

data Link = Link {output :: PwID, input :: PwID}

instance Show Link where
    show l = unwords [show l.output, show l.input]

-- | Node contains information about running media. Find the related ports through the nodeID attribute.
newtype Node = Node {media :: ByteString}
    deriving (Show)

newtype PwPath = PwPath {unPath :: ByteString}
    deriving newtype (Show, Eq)

data PwEvent
    = PwInput PwID Port
    | PwOutput PwID Port
    | PwNode PwID Node
    | PwLink PwID Link
    | PwRemoved PwID
    | PwDevice PwID Device
    deriving (Show)

data State = State
    { inputs :: IntMap Port
    , outputs :: IntMap Port
    , nodes :: IntMap Node
    , devices :: IntMap Device
    , links :: IntMap Link
    }

instance Show State where
    show s =
        mconcat
            [ "State{"
            , intercalate
                ","
                [ "ouputs=" <> (show . IM.toList) s.outputs
                , "inputs=" <> (show . IM.toList) s.inputs
                , "links=" <> (show . IM.toList) s.links
                ]
            , "}"
            ]

initialState :: State
initialState = State mempty mempty mempty mempty mempty

showState :: State -> ByteString
showState state =
    BS.unlines
        [ "outputs:"
        , showIM state.outputs showPort
        , "inputs:"
        , showIM state.inputs showPort
        , "medias:"
        , showIM state.nodes (.media)
        , "links:"
        , showIM state.links showLink
        , "devices:"
        , showIM state.devices (.name)
        ]
  where
    showIM im f = BS.unlines $ map showInfo $ IM.toList im
      where
        showInfo (pwid, obj) = BS.unwords ["-", showBS pwid <> ":", f obj]
    showPort port = BS.unwords [showBS port.nodeID, port.path.unPath]
    showLink link' = BS.unwords [showBS link'.output, "->", showBS link'.input]
    showBS :: (Show a) => a -> ByteString
    showBS = BS.pack . show

processEvent :: PwEvent -> State -> State
processEvent pwevent state = case pwevent of
    PwRemoved (PwID pwid) ->
        State
            { inputs = IM.delete pwid state.inputs
            , outputs = IM.delete pwid state.outputs
            , nodes = IM.delete pwid state.nodes
            , links = IM.delete pwid state.links
            , devices = IM.delete pwid state.devices
            }
    PwInput (PwID pwid) port -> state{inputs = IM.insert pwid port state.inputs}
    PwOutput (PwID pwid) port -> state{outputs = IM.insert pwid port state.outputs}
    PwLink (PwID pwid) link' -> state{links = IM.insert pwid link' state.links}
    PwNode (PwID pwid) media -> state{nodes = IM.insert pwid media state.nodes}
    PwDevice (PwID pwid) device -> state{devices = IM.insert pwid device state.devices}
