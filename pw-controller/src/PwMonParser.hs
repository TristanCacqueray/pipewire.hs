-- | This module is in charge of parsing pw-mon output
module PwMonParser (
    eventParser,
    introParser,
)
where

import Data.Attoparsec.ByteString.Char8 ((<?>))
import Data.Attoparsec.ByteString.Char8 qualified as Atto
import Data.ByteString qualified as BS hiding (pack)
import Data.List (find)
import RIO

import PwState

{- |

>>> Atto.parseOnly eventParser "removed:\n  id: 42\n\n"
Right (Right (PwRemoved 42))
-}
eventParser :: Atto.Parser (Either Text PwEvent)
eventParser = eventType <$> rawEventParser

introParser :: Atto.Parser [ByteString]
introParser = untilNewLine

eventType :: RawEvent -> Either Text PwEvent
eventType pwevent = case pwevent.typ of
    Added ->
        getType `orDie` "No type found" >>= \case
            "PipeWire:Interface:Port" -> do
                dir <- getDir
                nodeID <- getNodeID `orDie` "No node id found"
                path <- getPath `orDie` "No path found"
                kind <- getPortKind `orDie` "No kind found"
                pure $ dir Port{nodeID, kind, path}
            "PipeWire:Interface:Link" -> handleLink
            "PipeWire:Interface:Device" -> do
                name <- txtAttr " \t\tdevice.nick = \"" `orDie` "Device without nick"
                pure $ PwDevice pwevent.pwid Device{name}
            "PipeWire:Interface:Node" -> handleNewNode
            typ -> Left $ "Unknown type: " <> RIO.decodeUtf8Lenient typ
    Removed -> Right $ PwRemoved pwevent.pwid
    Changed ->
        getType `orDie` "No type found" >>= \case
            "PipeWire:Interface:Link" -> handleLink
            "PipeWire:Interface:Node" -> handleNode
            typ -> Left $ "Unknown changed type: " <> RIO.decodeUtf8Lenient typ
  where
    mkNode media = PwNode pwevent.pwid $ Node $ BS.copy media
    txtAttr :: ByteString -> Maybe ByteString
    txtAttr bs = BS.dropEnd 1 <$> attr bs

    handleNewNode = do
        media <- txtAttr " \t\tmedia.name = \"" `orDie` "node without media"
        pure $ mkNode media

    handleNode = do
        media <- txtAttr "*\t\tmedia.name = \"" `orDie` "No media found"
        pure $ mkNode media

    handleLink = do
        pwin <- getPort "\tinput-port-id: " `orDie` "No input found"
        pwout <- getPort "\toutput-port-id: " `orDie` "No input found"
        pure $ PwLink pwevent.pwid $ Link pwout pwin

    getPortKind :: Maybe PortKind
    getPortKind = case txtAttr " \t\taudio.channel = \"" of
        Just "FL" -> Just AudioFL
        Just "FR" -> Just AudioFR
        Just "MONO" -> Just AudioMono
        Just{} -> Nothing
        Nothing -> Just Midi -- Assume this is midi...
    getNodeID :: Maybe PwID
    getNodeID = fmap PwID . parseInt =<< attr " \t\tnode.id = \""

    getPort :: ByteString -> Maybe PwID
    getPort dir = fmap PwID . parseInt =<< attr dir

    getDir :: Either Text (Port -> PwEvent)
    getDir = do
        dir <- attr "\tdirection: " `orDie` "No direction found"
        case dir of
            "\"input\"" -> Right $ PwInput pwevent.pwid
            "\"output\"" -> Right $ PwOutput pwevent.pwid
            _ -> Left $ "Unknown direction" <> RIO.decodeUtf8Lenient dir
    getPath :: Maybe PwPath
    getPath = do
        value <- attr " \t\tport.alias = \"" <|> attr " \t\tobject.path = \""
        pure $ PwPath $ BS.copy $ BS.dropEnd 1 value
    getType :: Maybe ByteString
    getType = do
        value <- attr "\ttype: "
        pure $ fst $ BS.breakSubstring " (version " value
    attr :: ByteString -> Maybe ByteString
    attr name = do
        line <- Data.List.find (BS.isPrefixOf name) pwevent.attrs
        pure $ BS.drop (BS.length name) line

data RawEventType = Added | Removed | Changed
    deriving (Show)

data RawEvent = RawEvent
    { typ :: RawEventType
    , pwid :: PwID
    , attrs :: [ByteString]
    }
    deriving (Show)

{- |

>>> Atto.parseOnly lineParser "hello world\n\nrest"
Right "hello world"
-}
lineParser :: Atto.Parser ByteString
lineParser = do
    line <- Atto.takeWhile (/= '\n')
    _ <- Atto.char '\n'
    pure line

untilNewLine :: Atto.Parser [ByteString]
untilNewLine = go []
  where
    go acc = do
        line <- lineParser
        if line == mempty
            then pure (reverse acc)
            else
                if "\"{" `BS.isSuffixOf` line
                    then untilEndOfBlock acc
                    else go (line : acc)
    untilEndOfBlock acc = do
        line <- lineParser
        let newAcc = line : acc
        if "}\"" `BS.isSuffixOf` line
            then go newAcc
            else untilEndOfBlock newAcc

typeParser :: Atto.Parser RawEventType
typeParser = do
    eventName <- Atto.takeWhile (/= ':')
    _ <- Atto.string ":\n"
    case eventName of
        "added" -> pure Added
        "removed" -> pure Removed
        "changed" -> pure Changed
        n -> fail ("Unpexted type: " <> show n)

idParser :: Atto.Parser PwID
idParser = do
    Atto.skipSpace
    _ <- "id: "
    pwid <- PwID <$> Atto.decimal
    _ <- Atto.char '\n'
    pure pwid

rawEventParser :: Atto.Parser RawEvent
rawEventParser = do
    typ <- typeParser <?> "event type"
    pwid <- idParser <?> "event id"
    attrs <- untilNewLine <?> "event attrs"
    pure RawEvent{typ, pwid, attrs}

{- | Parse a int, ignoring extra trailing

>>> parseInt "42 "
Just 42

>>> parseInt "hello"
Nothing
-}
parseInt :: ByteString -> Maybe Int
parseInt bs = case Atto.parseOnly Atto.decimal bs of
    Left _ -> Nothing
    Right x -> Just x

orDie :: Maybe a -> Text -> Either Text a
orDie Nothing txt = Left txt
orDie (Just a) _ = Right a
