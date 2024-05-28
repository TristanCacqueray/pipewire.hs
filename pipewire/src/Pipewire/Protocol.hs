module Pipewire.Protocol where

newtype SeqID = SeqID {getID :: Int}
    deriving newtype (Show, Eq, Ord)

newtype PwID = PwID {getID :: Int}
    deriving newtype (Show, Eq, Ord)

newtype PwVersion = PwVersion {getVersion :: Int}
    deriving newtype (Show, Eq, Ord)
