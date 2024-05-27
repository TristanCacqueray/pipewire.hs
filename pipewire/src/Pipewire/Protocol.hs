module Pipewire.Protocol where

newtype SeqID = SeqID {getID :: Int}
    deriving newtype (Show)

newtype PwID = PwID {getID :: Int}
    deriving newtype (Show)

newtype PwVersion = PwVersion {getVersion :: Int}
    deriving newtype (Show)
