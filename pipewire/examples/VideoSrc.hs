{- | Draw a video source, and automatically connect the video source

This is presently a bit messy, see 'withAutoConnect'.
Ideally we would like to connect the source node during the pw_stream stateHandler,
where we can get the nodeid.

Unfortunately, when this handler is called on the first time, the port is not yet
created so we can't create the link directly.

So this example setups a custom registry handler to find the related portid and we create the
link connection there.
-}
module Main (main) where

import Control.Monad
import Data.IORef
import Foreign
import Foreign.C.Types (CInt)

import Pipewire qualified as PW
import Pipewire.RegistryState qualified as PW
import Pipewire.Video qualified as PW

main :: IO ()
main = withAutoConnect \nodeIDRef pwInstance -> do
    -- Store the node-id when the stream state changed
    let stateHandler stream = \case
            PW.PW_STREAM_STATE_PAUSED -> do
                -- when the node is created, a paused state change happens
                nodeID <- PW.pw_stream_get_node_id stream
                putStrLn $ "Stream state paused, node-id: " <> show nodeID
                -- share the node-id with the registry handler
                writeIORef nodeIDRef (Just nodeID)
            state -> putStrLn $ "Stream state changed: " <> show state

    -- Create a counter to animate the draw
    iTimeRef <- newIORef 0
    PW.withVideoStream pwInstance.mainLoop pwInstance.core stateHandler (draw iTimeRef) \stream -> do
        PW.connectVideoStream stream
        putStrLn "The stream is running!"
        -- Run the main loop forever.
        print =<< PW.runInstance pwInstance

type PixelRGBA = Word32

-- | Draw a gradient
draw :: IORef Word -> Ptr PixelRGBA -> CInt -> CInt -> IO ()
draw iTimeRef dataPtr width height = do
    -- update the counter to advance the animation time
    modifyIORef' iTimeRef (+ 1)
    iTime <- readIORef iTimeRef

    let writePixel x y pixel =
            let pos = 4 * (x + y * width)
             in poke (dataPtr `plusPtr` fromIntegral pos) pixel

    forM_ [0 .. height] \y ->
        forM_ [0 .. width] \x ->
            let red = (fromIntegral x / fromIntegral width)
                green = (fromIntegral y / fromIntegral height)
                blue = 0.5 + 0.5 * sin (fromIntegral iTime / 10)
             in writePixel x y (rgb red green blue)

-- | Create a pixel from value between 0.0 and 1.0
rgb :: Float -> Float -> Float -> PixelRGBA
rgb r g b =
    round (255 * r)
        .|. (round (255 * g) `shift` 8)
        .|. (round (255 * b) `shift` 16)
        .|. (a `shift` 24)
  where
    a = 0xff

-- | The node-id of the sink, to be started separately.
getSinkNode :: PW.RegistryState -> Either String PW.PwID
getSinkNode reg = case PW.findNode "video-sink" reg of
    Nothing -> Left $ "Start the sink with:\n " <> sinkCommand
    Just (nodeID, _) -> Right nodeID
  where
    sinkCommand = "gst-launch-1.0 pipewiresrc autoconnect=0 client-name=video-sink ! videoconvert ! autovideosink"

-- | This wrapper automatically connects the node-id when the port is added to the registry.
withAutoConnect :: (IORef (Maybe PW.PwID) -> PW.PwInstance PW.RegistryState -> IO a) -> IO a
withAutoConnect cb = do
    -- TODO: quit the loop if the link state is error?
    let linkInfoHandler _pwid linkStatus = do
            putStrLn $ "Link status: " <> show linkStatus

    -- The node-id reference shared with the stream handler
    nodeIDRef <- newIORef Nothing

    -- Setup the handler for the link events
    PW.withSpaHook \spaHook -> PW.withLinkEvents linkInfoHandler \pwLinkEvents -> do
        let
            -- Create the link between the ports
            connectPort pwInstance out inp = do
                putStrLn $ "Connecting... " <> show out <> " -> " <> show inp
                props <- PW.newLinkProperties $ PW.LinkProperties out inp False
                link <- PW.pw_link_create pwInstance.core props
                PW.pw_proxy_add_object_listener link.getProxy spaHook (PW.pwLinkEventsFuncs pwLinkEvents)

            -- Connect the node-id to the sink node
            connectSink reg pwInstance nodeID = do
                case PW.getLinkFrom nodeID reg of
                    -- The node-id has no existing links
                    [] -> case getSinkNode reg of
                        -- The sink exists
                        Right sinkID -> case PW.getLinkablePorts nodeID sinkID reg of
                            -- The node-id and the sink has 1 port each
                            [(outPort, inpPort)] -> connectPort pwInstance outPort inpPort
                            xs -> putStrLn $ "Could not find matching ports: " <> show xs
                        Left err -> putStrLn $ "Could not find the sink: " <> err
                    _ -> putStrLn "Already connected!"

            -- When a new port is added, try to connect the sink
            updateState pwInstance ev state = do
                reg <- PW.updateRegistryState ev state
                case ev of
                    PW.Added _ "PipeWire:Interface:Port" _ -> do
                        readIORef nodeIDRef >>= \case
                            Just nodeID -> connectSink reg pwInstance nodeID
                            -- We don't know the node-id yet
                            Nothing -> pure ()
                    _ -> pure ()
                pure reg

        -- Setup the instance and call the closure.
        PW.withInstance PW.initialRegistryState updateState (cb nodeIDRef)
