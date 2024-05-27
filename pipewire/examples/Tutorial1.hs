-- | https://docs.pipewire.org/tutorial1_8c-example.html
module Main (main) where

import Text.Printf

import Pipewire qualified as PW

main :: IO ()
main = PW.withPipewire do
    header <- PW.getHeadersVersion
    lib <- PW.getLibraryVersion
    putStrLn $ printf "Compiled with libpipewire %s\nLinked with libpipewire %s" header lib
