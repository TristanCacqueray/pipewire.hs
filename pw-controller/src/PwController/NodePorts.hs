module PwController.NodePorts where

import Data.List (foldl')
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map

import Pipewire (PwID)
import Pipewire.IDMap qualified as IDM
import Pipewire.RegistryState as Pw

import PwController.Matcher

-- | The evaluation process works at the node level.
data NodePorts = NodePorts
    { outputs :: [(PwID, Pw.Port)]
    , inputs :: [(PwID, Pw.Port)]
    }
    deriving (Show)

getNodeID :: NodePorts -> Maybe PwID
getNodeID nodePorts = case (nodePorts.outputs, nodePorts.inputs) of
    ((_, port) : _, _) -> Just port.nodeID
    (_, (_, port) : _) -> Just port.nodeID
    _ -> Nothing

addNodePortsInput, addNodePortsOutput :: (PwID, Pw.Port) -> NodePorts -> NodePorts
addNodePortsInput p (NodePorts out inp) = NodePorts out (p : inp)
addNodePortsOutput p (NodePorts out inp) = NodePorts (p : out) inp

-- | Group matching ports per node.
getPortMatcher :: Matcher -> RegistryState -> [NodePorts]
getPortMatcher matcher state = Map.elems nodes
  where
    inputs :: Map PwID NodePorts
    inputs = IDM.foldl' (addPorts addNodePortsInput) mempty state.inputs
    nodes = IDM.foldl' (addPorts addNodePortsOutput) inputs state.outputs
    addPorts set acc p@(_, port)
        | isMatch matcher port.alias =
            let add v = Just $ case v of
                    Nothing -> set p $ NodePorts [] []
                    Just nodePorts -> set p nodePorts
             in Map.alter add port.nodeID acc
        | otherwise = acc

getNodeFromLinks :: (PwID -> Bool) -> RegistryState -> [(PwID, PwID)] -> [NodePorts]
getNodeFromLinks isGoodNode state = Map.elems . foldl' addPorts mempty
  where
    addPorts :: Map PwID NodePorts -> (PwID, PwID) -> Map PwID NodePorts
    addPorts acc (out, inp) = case (IDM.lookup out state.outputs, IDM.lookup inp state.inputs) of
        (Just outPort, _) | isGoodNode outPort.nodeID -> addPort addNodePortsOutput (out, outPort)
        (_, Just inPort) | isGoodNode inPort.nodeID -> addPort addNodePortsInput (inp, inPort)
        _ -> acc
      where
        addPort set p@(_, port) =
            let add v = Just $ case v of
                    Nothing -> set p $ NodePorts [] []
                    Just nodePorts -> set p nodePorts
             in Map.alter add port.nodeID acc

getDeviceMatcher :: Matcher -> RegistryState -> Maybe [NodePorts]
getDeviceMatcher matcher state = case IDM.keep (\device -> isMatch matcher device.name) state.devices of
    [] -> Nothing
    xs -> getNodePred (\node -> any (\(devID, _) -> node.deviceID == Just devID) xs) state

getNodePred :: (Node -> Bool) -> RegistryState -> Maybe [NodePorts]
getNodePred pred' state = case IDM.keep pred' state.nodes of
    [] -> Nothing
    nodes -> Just $ map (getPorts . fst) nodes
  where
    getPorts nodeID =
        NodePorts
            { outputs = IDM.keep (\port -> port.nodeID == nodeID) state.outputs
            , inputs = IDM.keep (\port -> port.nodeID == nodeID) state.inputs
            }
