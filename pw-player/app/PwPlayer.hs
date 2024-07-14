-- | The program entry point
module Main (main) where

import Options.Applicative qualified as O
import Pipewire qualified as PW

-- | Command Line Interface
data Usage = Usage
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

main :: IO ()
main = do
    usage <- getUsage
    PW.withPipewire do
        print usage.debug
