-- | The program entry point
module Main (main) where

import Control.Monad (forM_, when)
import Control.Monad.ST (ST, runST)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import Data.STRef (STRef, newSTRef, readSTRef, writeSTRef)
import Data.Text (Text)
import Data.Vector.Storable qualified as SV
import Data.Vector.Storable.Mutable qualified as MV
import Options.Applicative qualified as O
import Pipewire qualified as PW
import Pipewire.Stream qualified as PW

newtype DemoState s = DemoState
    { pos :: STRef s Int
    }

-- | This module produces a sample for a given tick, see 'renderDemo'
demoModule :: DemoState s -> ST s Float
demoModule state = do
    -- Get the current position, restarting at 0 on overflow
    pos <- saturate demoLength <$> readSTRef state.pos

    -- Save the next position
    writeSTRef state.pos (pos + 1)

    -- Get the current pitch
    let pitch = getNote pos

    -- Render the sample
    pure $ 0.5 * mkPitch pitch pos

-- | Return the pitch of the current pos (44100 is 1sec)
getNote :: Int -> Float
getNote pos
    | pos > 20_000 = 500
    | pos > 12_000 = 480
    | otherwise = 440

-- | Render a pitch
mkPitch :: Float -> Int -> Float
mkPitch pitch pos = cos $ fromIntegral pos * pi * 2 * pitch / 44100

-- | Render the whole buffer
renderDemo :: SV.Vector Float
renderDemo = runST do
    state <- newDemoState
    -- Generate the buffer by calling the demoModule repeatedly
    SV.generateM demoLength $ const $ demoModule state

demoLength :: Int
demoLength = 44100

newDemoState :: ST s (DemoState s)
newDemoState = DemoState <$> newSTRef 0

saturate :: Int -> Int -> Int
saturate max' v
    | v > max' = 0
    | otherwise = v

-- | Command Line Interface
newtype Usage = Usage
    { debug :: Bool
    }

usageParser :: O.Parser Usage
usageParser =
    Usage
        <$> O.switch (O.long "debug" <> O.help "Debug internal state")

getUsage :: IO Usage
getUsage = O.execParser opts
  where
    opts =
        O.info
            (usageParser O.<**> O.helper)
            (O.fullDesc <> O.header "pw-player - a pipewire audio player")

data Stem = Stem
    { name :: Text
    , samples :: SV.Vector Float
    , position :: IORef Int
    }

data Player = Player
    { chans :: PW.Channels
    , rate :: PW.Rate
    , stems :: IORef [Stem]
    }

newPlayer :: IO Player
newPlayer = Player (PW.Channels 1) (PW.Rate 44100) <$> newIORef []

newStem :: Text -> SV.Vector Float -> IO Stem
newStem name samples = Stem name samples <$> newIORef 0

playStem :: Player -> Stem -> IO ()
playStem player stem = modifyIORef' player.stems (stem :)

startPlayer :: Player -> IO ()
startPlayer player = PW.withMainLoop \mainLoop -> do
    loop <- PW.pw_main_loop_get_loop mainLoop
    PW.withContext loop \context -> PW.withCore context \core -> do
        let cb = PW.simpleAudioHandler player.chans (onProcess mainLoop)
        PW.withAudioStream core cb \stream -> do
            PW.connectAudioStream player.rate player.chans stream
            PW.pw_main_loop_run mainLoop
  where
    PW.Channels chanCount = player.chans
    onProcess :: PW.MainLoop -> Int -> IO (SV.Vector Float)
    onProcess loop frames =
        readIORef player.stems >>= \case
            [] -> do
                putStrLn "Nothing to play..."
                pure mempty
            [stem] -> do
                -- Read current playback position
                pos <- readIORef stem.position

                -- Compute the end pos
                let endPos = pos + frames * chanCount

                -- Update the playback position for next callback
                writeIORef stem.position endPos

                let size = min endPos (SV.length stem.samples) - pos

                if size > 0
                    then pure $ SV.slice pos size stem.samples
                    else do
                        -- Reached the end of file
                        PW.pw_main_loop_quit loop
                        pure mempty
            _ -> error "mixing multiple stems is not implemented"

main :: IO ()
main = do
    usage <- getUsage
    PW.withPipewire do
        player <- newPlayer
        -- print $ SV.take 200 renderDemo'
        -- error "stop"
        playStem player =<< newStem "demo" renderDemo
        when usage.debug do
            putStrLn "Running"
        startPlayer player
