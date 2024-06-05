module Main (main) where

import System.Environment (getArgs)

import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Vector.Storable qualified as SV
import Sound.File.Sndfile qualified as SF
import Sound.File.Sndfile.Buffer.Vector qualified as BV

import Pipewire qualified as PW
import Pipewire.Stream qualified as PW

decodeSndFile :: FilePath -> IO (PW.Rate, PW.Channels, SV.Vector Float)
decodeSndFile fp = do
    (info, Just x) <- SF.readFile fp
    pure (PW.Rate info.samplerate, PW.Channels info.channels, BV.fromBuffer x)

main :: IO ()
main =
    getArgs >>= \case
        [fp] -> decodeSndFile fp >>= play
        _ -> putStrLn "usage: hs-pw-play filename"

play :: (PW.Rate, PW.Channels, SV.Vector Float) -> IO ()
play (rate, chans, samples) = PW.withPipewire $ PW.withMainLoop \mainLoop -> do
    loop <- PW.pw_main_loop_get_loop mainLoop
    PW.withContext loop \context -> PW.withCore context \core -> do
        -- The current playback position in the samples buffer
        posRef <- newIORef 0

        -- Create the stream and install the onProcess callback
        let cb = PW.simpleAudioHandler chans (onProcess mainLoop posRef)
        PW.withAudioStream core cb \stream -> do
            -- Start the stream
            PW.connectAudioStream rate chans stream

            -- Wait until the loop stops
            putStrLn "Playing..."
            PW.pw_main_loop_run mainLoop
  where
    PW.Channels chanCount = chans
    onProcess :: PW.PwMainLoop -> IORef Int -> Int -> IO (SV.Vector Float)
    onProcess loop posRef frames = do
        -- Read current playback position
        pos <- readIORef posRef

        -- Compute the end pos
        let endPos = pos + frames * chanCount

        -- Update the playback position for next callback
        writeIORef posRef endPos

        let size = min endPos (SV.length samples) - pos

        if size > 0
            then pure $ SV.slice pos size samples
            else do
                -- Reached the end of file
                PW.pw_main_loop_quit loop
                pure mempty
