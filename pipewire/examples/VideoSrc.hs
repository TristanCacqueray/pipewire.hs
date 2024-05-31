-- | Draw a video source
module Main (main) where

import Control.Monad
import Data.IORef
import Foreign
import Foreign.C.Types (CInt)

import Pipewire qualified as PW
import Pipewire.Instance qualified as PW
import Pipewire.Video qualified as PW

main :: IO ()
main = PW.withLinkInstance \pwInstance -> do
    iTimeRef <- newIORef 0
    let stateHandler _stream state = do
            putStrLn $ "Stream state changed: " <> show state
    PW.withVideoStream pwInstance.mainLoop pwInstance.core stateHandler (draw iTimeRef) \stream -> do
        PW.connectVideoStream stream
        putStrLn "Running!"
        print =<< PW.runInstance pwInstance
  where
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
