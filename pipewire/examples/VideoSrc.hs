{- | Draw a video source, and automatically connect the video source

This is presently a bit messy, see 'withAutoConnect'.
Ideally we would like to connect the source node during the pw_stream stateHandler,
where we can get the nodeid.

Unfortunately, when this handler is called on the first time, the port is not yet
created so we can't create the link directly.

So this example setups a custom registry handler to find the related portid and we create the
link connection there. This needs passing the NodeID through the nodeIDRef.
-}
module Main (main) where

import Control.Monad
import Data.IORef
import Foreign
import Foreign.C.Types (CInt)

import Pipewire qualified as PW
import Pipewire.Instance qualified as PW
import Pipewire.Video qualified as PW

main :: IO ()
main = withAutoConnect \nodeIDRef pwInstance -> do
    let stateHandler stream = \case
            PW.PW_STREAM_STATE_PAUSED -> do
                nodeID <- PW.pw_stream_get_node_id stream
                writeIORef nodeIDRef (Just nodeID)
                putStrLn $ "Got node-id: " <> show nodeID
            state -> putStrLn $ "Stream state changed: " <> show state
    iTimeRef <- newIORef 0
    PW.withVideoStream pwInstance.mainLoop pwInstance.core stateHandler (draw iTimeRef) \stream -> do
        PW.connectVideoStream stream
        putStrLn "Running!"
        print =<< PW.runInstance pwInstance

draw :: IORef Word -> Ptr Word32 -> CInt -> CInt -> IO ()
draw iTimeRef dataPtr width height = do
    modifyIORef' iTimeRef (+ 1)
    iTime <- readIORef iTimeRef
    let blue = 0.5 + 0.5 * sin (fromIntegral iTime / 30)

    forM_ [0 .. height] \y ->
        forM_ [0 .. width] \x ->
            writePixel width x y (fromIntegral x / fromIntegral width) 0 blue dataPtr

writePixel :: CInt -> CInt -> CInt -> Float -> Float -> Float -> Ptr Word32 -> IO ()
writePixel width x y r g b dataPtr =
    poke (dataPtr `plusPtr` fromIntegral pos) pixel
  where
    pixel :: Word32
    pixel =
        round (255 * r)
            .|. (round (255 * g) `shift` 8)
            .|. (round (255 * b) `shift` 16)
            .|. (a `shift` 24)
    a = 0xff
    pos = 4 * (x + y * width)

getSinkNode :: PW.LinksRegistry -> Either String PW.PwID
getSinkNode reg = case PW.findNode "video-sink" reg of
    Nothing -> Left "Start gst-launch-1.0 pipewiresrc autoconnect=0 client-name=video-sink ! videoconvert ! autovideosink"
    Just (nodeID, _) -> Right nodeID

withAutoConnect :: (IORef (Maybe PW.PwID) -> PW.PwInstance PW.LinksRegistry -> IO a) -> IO a
withAutoConnect cb = do
    let linkInfoHandler _pwid linkStatus = do
            putStrLn $ "Link status: " <> show linkStatus

    PW.with_spa_hook \spaHook -> PW.withPwLinkEvents linkInfoHandler \pwLinkEvents -> do
        let connectPort pwInstance out inp = do
                putStrLn $ "Connecting... " <> show out <> " -> " <> show inp
                props <- PW.newLinkProperties $ PW.LinkProperties out inp False
                link <- PW.pw_link_create pwInstance.core props
                PW.pw_proxy_add_object_listener link.getProxy spaHook (PW.pwLinkEventsFuncs pwLinkEvents)
                pure ()
        let connectSink reg pwInstance nodeID = do
                case PW.getLinkFrom nodeID reg of
                    [] -> case getSinkNode reg of
                        Left err -> putStrLn err
                        Right sinkID -> case PW.getLinkablePorts nodeID sinkID reg of
                            [(outPort, inpPort)] -> connectPort pwInstance outPort inpPort
                            _ -> putStrLn "Could not find matching ports"
                    _ -> putStrLn "Already connected!"

        nodeIDRef <- newIORef Nothing
        let updateState pwInstance ev state = do
                reg <- PW.updateLinksRegistry ev state
                case ev of
                    PW.Added _ "PipeWire:Interface:Port" _ -> do
                        readIORef nodeIDRef >>= \case
                            Just nodeID -> connectSink reg pwInstance nodeID
                            Nothing -> putStrLn "no node-id"
                    _ -> pure ()
                pure reg
        PW.withInstance PW.initialLinksRegistry updateState (cb nodeIDRef)
