module PwController.Eval where

import Data.Either (partitionEithers)
import Data.List (foldl', sortBy)
import Data.Maybe (isNothing)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text

import Pipewire (PwID)
import Pipewire.IDMap qualified as IDM
import Pipewire.RegistryState as Pw

import PwController.Expr as Expr
import PwController.Matcher
import PwController.NodePorts

data EvalError = LinkError Text | TargetMissing Target | TargetHasNoPort Target | BadRedirect Target
    deriving (Show)

data Action = AddLink (PwID, PwID) | DelLink PwID | Err EvalError

simplify :: [Action] -> [Action]
simplify xs = errors' <> (DelLink <$> Set.toList deleted') <> (AddLink <$> Set.toList added')
  where
    (deleted', added', errors') = foldl' go (mempty, mempty, mempty) xs
    go (deleted, added, errors) = \case
        AddLink l -> (deleted, Set.insert l added, errors)
        DelLink l -> (Set.insert l deleted, added, errors)
        e -> (deleted, added, e : errors)

-- | Resolve a target into a list of 'NodePorts'.
getTarget :: Target -> RegistryState -> Either EvalError [NodePorts]
getTarget target@(Target kind matcher) state = case mPorts of
    Nothing -> Left (TargetMissing target)
    Just [] -> Left (TargetHasNoPort target)
    Just xs -> Right xs
  where
    mPorts = case kind of
        Expr.Node -> getNodePred (\node -> isMatch matcher node.name) state
        Expr.Device -> getDeviceMatcher matcher state
        Expr.Port -> Just $ getPortMatcher matcher state
        Expr.Media -> getNodePred matchMedia state
    matchMedia node = case node.media of
        Just m -> isMatch matcher m
        Nothing -> False

getLinks :: [(PwID, Pw.Port)] -> [(PwID, Pw.Port)] -> Either Text [(PwID, PwID)]
getLinks [] _ = Left "No target ports"
getLinks _ [] = Left "No other ports"
-- The simplest case: a single link
getLinks [(tid, target)] [(oid, other)] = case (target.kind, other.kind) of
    (Audio _, Audio _) -> Right [(tid, oid)]
    (Midi, Midi) -> Right [(tid, oid)]
    _ -> Left $ "Incompatible ports: " <> tshow (target, other)
-- One to many: a single target port connected to multiple other ports
getLinks [(tid, target)] xs = case target.kind of
    AudioMono
        | -- Try mono -> mono
          Just oid <- getPort (== AudioMono) xs ->
            pure [(tid, oid)]
        | -- Try mono -> stereo
          Just (lid, rid) <- getPortStereo xs ->
            pure [(tid, lid), (tid, rid)]
        | -- Try mono -> any port
          Just oid <- getPort isAudio xs ->
            pure [(tid, oid)]
        | otherwise -> Left $ "No matching mono port: " <> tshow (target, xs)
    Audio Nothing
        | -- Try the first audio without channel
          Just oid <- getPort (== target.kind) xs ->
            pure [(tid, oid)]
        | -- Try to connect to the first audio channel
          Just oid <- getPort isAudio xs ->
            pure [(tid, oid)]
        | otherwise -> Left $ "No matching audio port: " <> tshow (target, xs)
    kind
        | Just oid <- getPort (== kind) xs -> pure [(tid, oid)]
        | otherwise -> Left $ "No matching port: " <> tshow (kind, xs)
-- Many to many
getLinks targets xs = go (sortPorts targets) (sortPorts xs) []
  where
    go [] _ [] = Left $ "Could not find a single matching ports: " <> tshow (targets, xs)
    go [] _ acc = pure $ reverse acc
    go ((tid, target) : rest) others acc = case getPort matchingPort others of
        Just oid ->
            -- Connect the matching port and remove it from the list of other available ports
            go rest (removePort oid others) ((tid, oid) : acc)
        Nothing ->
            -- Ignore port that can't be matched, should this be an error?
            go rest others acc
      where
        matchingPort = case target.kind of
            -- Audio without channel match any audio port
            Audio Nothing -> isAudio
            -- Otherwise the port needs to be identical (e.g. left with left, mono with mono, ...)
            kind -> (== kind)
    removePort p = filter (\(pwid, _) -> pwid /= p)

sortPorts :: [(PwID, Pw.Port)] -> [(PwID, Pw.Port)]
sortPorts = sortBy cmp
  where
    cmp (_, a) (_, b) = case compare a.kind b.kind of
        EQ -> compare a.alias b.alias
        ord -> ord

isAudio :: PortKind -> Bool
isAudio = \case
    Audio{} -> True
    _ -> False

getPort :: (PortKind -> Bool) -> [(PwID, Pw.Port)] -> Maybe PwID
getPort _ [] = Nothing
getPort pred' ((pwid, port) : rest)
    | pred' port.kind = Just pwid
    | otherwise = getPort pred' rest

getPortStereo :: [(PwID, Pw.Port)] -> Maybe (PwID, PwID)
getPortStereo xs = (,) <$> getPort (== AudioFL) xs <*> getPort (== AudioFR) xs

getLinkPairs :: Direction -> NodePorts -> NodePorts -> Either Text [(PwID, PwID)]
getLinkPairs direction target with = case direction of
    Input -> getInputs
    Output -> getOutputs
    InOut -> do
        inputs <- getInputs
        outputs <- getOutputs
        pure $ inputs <> outputs
  where
    getInputs = map (\(a, b) -> (b, a)) <$> getLinks target.inputs with.outputs
    getOutputs = getLinks target.outputs with.inputs

debugTarget :: RegistryState -> Rules -> [(Target, Either EvalError [NodePorts])]
debugTarget state = concatMap debugExpr
  where
    debugExpr = \case
        When{body} -> concatMap debugExpr body
        Disconnect{target} -> [(target, getTarget target state)]
        Connect{target, with} -> [(target, getTarget target state), (with, getTarget with state)]

applyConfig :: RegistryState -> Rules -> [Action]
applyConfig state = simplify . concatMap evalExpr
  where
    evalExpr :: Expr -> [Action]
    evalExpr = \case
        When{condition, body}
            | evalCondition condition -> concatMap evalExpr body
            | otherwise -> []
        Disconnect{direction, target} -> evalTargetPorts do
            targets <- getTarget target state
            pure $ DelLink <$> concatMap (nodeLinks direction) targets
        Connect{direction, target, with, redirect} -> evalTargetPorts do
            targets <- getTarget target state
            others <- case getTarget with state of
                Left err -> case redirect of
                    -- without redirect, a missing other is fatal
                    Nothing -> Left err
                    -- if there is a redirect, provides empty others
                    Just{} -> Right []
                Right xs -> Right xs
            let (addRedirectError, mRedirect) = case redirect of
                    Nothing -> (id, Nothing)
                    Just redirectTarget -> case getTarget redirectTarget state of
                        Left err -> ((Err err :), Nothing)
                        Right [x] -> (id, Just x)
                        Right _ -> ((Err (BadRedirect redirectTarget) :), Nothing)
            pure $ addRedirectError $ concatMap (connect direction mRedirect others) targets

    evalTargetPorts :: Either EvalError [Action] -> [Action]
    evalTargetPorts = \case
        Left err -> [Err err]
        Right xs -> xs

    connect :: Direction -> Maybe NodePorts -> [NodePorts] -> NodePorts -> [Action]
    connect direction mRedirect others target =
        (Err . LinkError <$> (redirectErrors <> errors))
            <> (DelLink <$> unwanted)
            <> (AddLink <$> (redirectNeeded <> needed))
      where
        redirectNeeded = filter (\pair -> not (IDM.contains (== pair) state.links)) redirectWanted
        (redirectErrors, concat -> redirectWanted) = case (mRedirect, getNodeID target, map snd unwantedLinks) of
            (_, Nothing, _) -> (["Target has no ports!"], [])
            (Just redirect, Just targetNodeID, xs@(_ : _)) ->
                partitionEithers
                    . map (getLinkPairs direction redirect)
                    $ getNodeFromLinks (/= targetNodeID) state xs
            _ -> ([], [])

        unwanted = map fst unwantedLinks
        unwantedLinks = case needed of
            -- If the wanted connection are set, then we don't disconnect anything
            [] | isNothing mRedirect -> []
            -- Otherwise we remove any other links
            _ -> IDM.keep isUnwanted state.links
        isUnwanted pair@(out, inp)
            | pair `elem` wanted = False
            | otherwise = case direction of
                Input -> isInput
                Output -> isOutput
                InOut -> isInput || isOutput
          where
            isInput = inp `elem` map fst target.inputs
            isOutput = out `elem` map fst target.outputs
        needed = filter (\pair -> not (IDM.contains (== pair) state.links)) wanted
        (errors, concat -> wanted) = partitionEithers $ map (getLinkPairs direction target) others

    isDeviceConnected matcher = IDM.contains (\dev -> isMatch matcher dev.name) state.devices

    evalCondition :: Condition -> Bool
    evalCondition = \case
        DeviceMatcher matcher -> isDeviceConnected matcher
        NodeMatcher _ -> error "TODO"

    -- \| Gets the id of the links connected to the 'NodePorts'
    nodeLinks :: Direction -> NodePorts -> [PwID]
    nodeLinks direction nodePorts = map fst $ IDM.keep nodeLink state.links
      where
        inputs = Set.fromList $ map fst nodePorts.inputs
        outputs = Set.fromList $ map fst nodePorts.outputs
        nodeLink (out, inp) = case direction of
            Input -> Set.member out outputs
            Output -> Set.member inp inputs
            InOut -> Set.member out outputs || Set.member inp inputs

tshow :: (Show a) => a -> Text
tshow = Text.pack . show
