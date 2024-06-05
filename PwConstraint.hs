-- | This module contains the logic to apply constraint to the state
module PwConstraint (
    Constraint,
    Action (..),
    parseConstraint,
    applyConstraints,
)
where

import Data.Attoparsec.ByteString ((<?>))
import Data.Attoparsec.ByteString.Char8 qualified as Atto
import Data.Coerce (coerce)
import Data.IntMap.Strict qualified as IM
import Data.List (find)
import Data.Set qualified as Set
import Data.Text qualified as Text
import RIO
import Text.Regex.PCRE.Light qualified as R

import PwState

data Condition
    = DeviceConnected Regex
    | Always
    deriving (Show)

data ReAction
    = SetOutput Regex Regex
    | SetInput Regex Regex
    | MediaOut Regex (Regex, Regex) (Maybe (Regex, Regex))
    | DisconnectInput Regex
    deriving (Show)

data Stm = Stm
    { condition :: Condition
    , actions :: [ReAction]
    }

instance Show Stm where
    show stm = unwords [show stm.condition, show stm.actions]

newtype Constraint = Constraint [Stm]

instance Show Constraint where
    show (Constraint xs) = show xs

data Action = AddLink (PwID, PwID) | DelLink PwID
    deriving (Show)

simplify :: [Action] -> [Action]
simplify xs = (DelLink <$> Set.toList deleted) <> (AddLink <$> Set.toList added)
  where
    (Set.fromList -> deleted, Set.fromList -> added) = partitionEithers $ map addedDeleted xs
    addedDeleted = \case
        AddLink l -> Right l
        DelLink l -> Left l

applyConstraints :: Constraint -> State -> [Action]
applyConstraints (Constraint xs) state = simplify $ concatMap evalStm xs
  where
    evalStm :: Stm -> [Action]
    evalStm stm
        | evalCondition stm.condition = concatMap applyConstraint stm.actions
        | otherwise = []

    applyConstraint :: ReAction -> [Action]
    applyConstraint = \case
        SetOutput outPort inpPort -> applyForcePort outPort inpPort findOutLinks
        SetInput inpPort outPort -> applyForcePort outPort inpPort findInpLinks
        MediaOut media inPorts otherPorts -> case (findInIDs inPorts, findInIDs =<< otherPorts) of
            (Just ports, other) -> applyForceMedia media ports other
            _ -> []
        DisconnectInput reg -> concatMap deleteOutputLinks $ findPorts outputs reg

    deleteOutputLinks :: PwID -> [Action]
    deleteOutputLinks outID = DelLink . fst <$> filter (\(_, l) -> l.output == outID) links

    nodes :: [(PwID, Node)]
    nodes = coerce $ IM.toList state.nodes
    applyForceMedia :: Regex -> (PwID, PwID) -> Maybe (PwID, PwID) -> [Action]
    applyForceMedia media (left, right) mOther = concatMap go $ mapMaybe getMediaOutput nodes
      where
        getMediaOutput (pwid, node) = do
            outs <- getMediaOut pwid
            -- traceShowM ("Matching", media, node.media, isMatch media node.media)
            Just $
                if isMatch media node.media
                    then Right outs
                    else Left outs
        -- ensure this media is connected to (left,right)
        go (Right (mediaLeft, mediaRight)) =
            ensure mediaLeft left findOutLinks <> ensure mediaRight right findOutLinks
        -- if this media is connected to (left,right), move it to (otherLeft,otherRight)
        go (Left (mediaLeft, mediaRight)) = case mOther of
            Nothing -> []
            Just (otherLeft, otherRight) -> replaceOutLink mediaLeft left otherLeft <> replaceOutLink mediaRight right otherRight

    replaceOutLink :: PwID -> PwID -> PwID -> [Action]
    replaceOutLink outID oldInID inID = case findLink outID oldInID of
        Nothing -> []
        Just pwID ->
            DelLink pwID : case findLink outID inID of
                Nothing -> [AddLink (outID, inID)]
                Just{} -> []

    -- Lookup the stereo port of a given media nodeID
    getMediaOut :: PwID -> Maybe (PwID, PwID)
    getMediaOut nodeID = case filter (\(_, p) -> p.nodeID == nodeID) outputs of
        [(a, aPort), (b, bPort)] -> case (aPort.kind, bPort.kind) of
            (AudioFL, AudioFR) -> Just (a, b)
            (AudioFR, AudioFL) -> Just (b, a)
            _ -> Nothing
        _ -> Nothing

    findInIDs :: (Regex, Regex) -> Maybe (PwID, PwID)
    findInIDs (inLeft, inRight) = (,) <$> findPort inputs inLeft <*> findPort inputs inRight

    applyForcePort :: Regex -> Regex -> (PwID -> PwID -> [Either PwID b]) -> [Action]
    applyForcePort outPort inpPort f = case (findPort outputs outPort, findPort inputs inpPort) of
        (Just out, Just inp) -> ensure out inp f
        _ -> []

    ensure :: PwID -> PwID -> (PwID -> PwID -> [Either PwID b]) -> [Action]
    ensure out inp f = case present of
        [] -> AddLink (out, inp) : unWanted
        _ -> unWanted
      where
        (extras, present) = partitionEithers $ f out inp
        unWanted = DelLink <$> extras

    links :: [(PwID, Link)]
    links = coerce $ IM.toList state.links
    findLink out inp = fst <$> find (\(_, l) -> l.output == out && l.input == inp) links

    findOutLinks :: PwID -> PwID -> [Either PwID PwID]
    findOutLinks out inp = mapMaybe isLinkMatch links
      where
        isLinkMatch (pwid, l)
            | l.output == out = Just $ if l.input == inp then Right pwid else Left pwid
            | otherwise = Nothing

    findInpLinks :: PwID -> PwID -> [Either PwID PwID]
    findInpLinks out inp = mapMaybe isLinkMatch links
      where
        isLinkMatch (pwid, l)
            | l.input == inp = Just $ if l.output == out then Right pwid else Left pwid
            | otherwise = Nothing

    inputs, outputs :: [(PwID, Port)]
    inputs = coerce $ IM.toList state.inputs
    outputs = coerce $ IM.toList state.outputs

    findPort :: [(PwID, Port)] -> Regex -> Maybe PwID
    findPort ports regex = case findPorts ports regex of
        (x : _) -> Just x
        _ -> Nothing

    findPorts :: [(PwID, Port)] -> Regex -> [PwID]
    findPorts ports regex = fst <$> filter (\(_, port) -> isMatch regex (coerce port.path)) ports

    devices :: [(PwID, Device)]
    devices = coerce $ IM.toList state.devices
    isDeviceConnected reg = isJust $ find (\d -> isMatch reg (snd d).name) devices

    evalCondition :: Condition -> Bool
    evalCondition = \case
        DeviceConnected reg -> isDeviceConnected reg
        Always -> True

{- | Skip empty lines and lines starting with '#'

>>> runP (skipComment *> parseString) "# test\n\n#comment\n  toto"
Right "toto"
-}
skipComment :: Atto.Parser ()
skipComment = Atto.skipMany do
    Atto.skipSpace
    Atto.anyChar >>= \case
        '\n' -> pure ()
        '#' -> Atto.skipWhile (/= '\n') *> void (Atto.char '\n')
        _ -> fail "not a comment"

{- | Parse a word or a quoted string

>>> runP parseString "  toto"
Right "toto"

>>> runP parseString "\"Hello world\""
Right "Hello world"
-}
parseString :: Atto.Parser ByteString
parseString = do
    Atto.skipSpace
    quoted <|> Atto.takeWhile (`notElem` [' ', '\n'])
  where
    quoted = Atto.char '"' *> Atto.takeWhile (/= '"') <* Atto.char '"'

{- |
>>> import Data.ByteString.Char8 as BS (unlines)
>>> parseConstraint $ BS.unlines ["# a conf", "always:", "  set output b:{1,2} \"a card:{L,R}\"", "# a comment", "", "# comment"]
Right [Always [SetOutput "b:1" "a card:L",SetOutput "b:2" "a card:R"]]

>>> parseConstraint $ BS.unlines ["when device:mini", "  set output a b", "  set input a d"]
Right [DeviceConnected "mini" [SetOutput "a" "b",SetInput "a" "d"]]
-}
parseConstraint :: ByteString -> Either Text Constraint
parseConstraint bs = Constraint <$> runP (Atto.many1' parser) bs
  where
    parser :: Atto.Parser Stm
    parser = do
        skipComment
        condition <- parseCondition <?> "condition"
        _ <- Atto.string "\n"
        actions <- concat <$> Atto.many1' parseActionLine <?> "actions"
        skipComment
        pure $ Stm{condition, actions}

    parseCondition =
        (Always <$ Atto.string "always:") <|> do
            _ <- Atto.string "when "
            Atto.string "device:" *> (DeviceConnected <$> parseDevice)

    parseDevice :: Atto.Parser Regex
    parseDevice = newRegex <$> parseString

    parseActionLine :: Atto.Parser [ReAction]
    parseActionLine = do
        skipComment
        _ <- Atto.char ' '
        Atto.skipSpace
        action <- parseAction
        _ <- Atto.char '\n'
        pure action

    parseAction =
        ("set output " *> parsePaths SetOutput)
            <|> ("set input " *> parsePaths SetInput)
            <|> ("media out " *> parseMediaOut)
            <|> ("disconnect input " *> ((\a -> [DisconnectInput (newRegex a)]) <$> parseString))

    parseMediaOut = do
        media <- newRegex <$> parseString
        out <- parseStereo
        other <- optional parseStereo
        pure [MediaOut media out other]

    parseStereo =
        parsePathArg >>= \case
            [a, b] -> pure (a, b)
            _ -> fail "not stereo"

    parsePaths ctor = do
        outPaths <- parsePathArg
        inPaths <- parsePathArg
        if length outPaths == length inPaths
            then pure $ zipWith ctor outPaths inPaths
            else fail "[extras] does not match"

    parsePathArg :: Atto.Parser [Regex]
    parsePathArg = do
        arg <- parseString
        case runP pathParser arg of
            Left e -> fail (show e)
            Right (base, extras) -> pure $ map (\extra -> newRegex (base <> extra)) extras

runP :: Atto.Parser a -> ByteString -> Either Text a
runP parser bs = either (Left . Text.pack) Right $ Atto.parseOnly (parser <* Atto.endOfInput) bs

pathParser :: Atto.Parser (ByteString, [ByteString])
pathParser = do
    base <- Atto.takeWhile1 (/= '{')
    Atto.peekChar >>= \case
        Just '{' -> do
            _ <- Atto.char '{'
            xs <- Atto.sepBy1 (Atto.takeWhile1 (`notElem` [',', '}'])) (Atto.char ',')
            _ <- Atto.char '}'
            pure (base, xs)
        _ -> pure (base, [""])

{- | Some examples doctest:

-- Setup a fake state:
>>> import Data.ByteString.Char8 as BS (pack, unlines)
>>> let mkIM base cons xs = IM.fromList (zip [base..] (cons <$> xs))
>>> let mkPort= Port 0 AudioFL . PwPath . BS.pack
>>> let mkS outputs inputs = State (mkIM (length outputs) mkPort inputs) (mkIM 0 mkPort outputs) mempty mempty mempty
>>> let state = mkS ["reaper:1", "reaper:2"] ["play_FL", "play_FR", "head_L"]
>>> state
State{ouputs=[(0,0|"reaper:1"),(1,0|"reaper:2")],inputs=[(2,0|"play_FL"),(3,0|"play_FR"),(4,0|"head_L")],links=[]}

-- Apply constraint:
>>> let mkC xs = (either (error . show) id . parseConstraint) $ BS.unlines xs
>>> applyConstraints (mkC ["always:", " set output reaper:{1,2} play_{FL,FR}"]) state
[AddLink (0,2),AddLink (1,3)]

-- Add unwanted link
>>> let addLink base xs s = s{links=IM.fromList (zip [base..] (uncurry Link <$> xs))}
>>> let s2 = addLink 10 [(0,2),(0,4)] state
>>> s2
State{ouputs=[(0,0|"reaper:1"),(1,0|"reaper:2")],inputs=[(2,0|"play_FL"),(3,0|"play_FR"),(4,0|"head_L")],links=[(10,0 2),(11,0 4)]}

>>> applyConstraints (mkC ["always:", " set output (rea|lea)per:1 play_FL"]) state
[AddLink (0,2)]
-}

-- | A Regex which show it's original value
data Regex = Regex {r :: R.Regex, name :: ByteString}

instance Show Regex where show r = show r.name

newRegex :: ByteString -> Regex
newRegex name = Regex{r = R.compile name [R.anchored], name}

{- | toto
>>> isMatch (newRegex "(Meet . .*|.* Jitsi Meet)") "Meet – hgr-iwkm-efk"
True
-}
isMatch :: Regex -> ByteString -> Bool
isMatch regex bs = isJust $ R.match regex.r bs []
