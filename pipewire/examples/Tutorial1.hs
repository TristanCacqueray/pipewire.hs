-- | https://docs.pipewire.org/tutorial1_8c-example.html
module Main (main) where

import Text.Printf

import Pipewire

main :: IO ()
main = do
    pwInit
    header <- pwGetHeadersVersion
    lib <- pwGetLibraryVersion
    putStrLn $ printf "Compiled with libpipewire %s\nLinked with libpipewire %s" header lib
