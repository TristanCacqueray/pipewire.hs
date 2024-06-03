-- | Playing A Tone - https://docs.pipewire.org/tutorial4_8c-example.html
module Main (main) where

import Data.IORef
import Data.Vector.Storable qualified as VS

import Pipewire qualified as PW
import Pipewire.Stream qualified as PW

main :: IO ()
main = PW.withPipewire $ PW.withMainLoop \mainLoop -> do
    loop <- PW.pw_main_loop_get_loop mainLoop
    accumulatorRef <- newIORef 0
    PW.withAudioStream loop (onProcess accumulatorRef) \stream -> do
        PW.connectAudioStream chans stream
        putStrLn "Running..."
        print =<< PW.pw_main_loop_run mainLoop
  where
    volume = 0.3
    chans = 1

    -- Return the current cos input
    sampleX pos = pos * pi * 2 * 440 / 44100

    -- The stream onProcess callback
    onProcess accumulatorRef pwStream = do
        -- Get the next buffer
        PW.pw_stream_dequeue_bufer pwStream >>= \case
            Just buffer -> onProcessBuffer accumulatorRef pwStream buffer
            Nothing -> putStrLn "out of buffer"

    onProcessBuffer accumulatorRef pwStream buffer = do
        -- Check how many samples is needed
        frames <- PW.audioFrames chans buffer
        -- putStrLn $ "On process called!, n_frames=" <> show frames

        -- Read the last accumulated value
        accumulator <- readIORef accumulatorRef

        -- Generate and write new samples
        PW.writeAudioFrame buffer $ VS.generate frames \pos ->
            volume * cos (accumulator + sampleX (fromIntegral pos))

        -- Update the accumulated value
        writeIORef accumulatorRef $ (accumulator + sampleX (fromIntegral frames))

        -- Submit the buffer
        PW.pw_stream_queue_buffer pwStream buffer
