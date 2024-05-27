module Pipewire.Protocol where

newtype SeqID = SeqID {getID :: Int}
    deriving newtype (Show, Eq)

newtype PwID = PwID {getID :: Int}
    deriving newtype (Show, Eq)

newtype PwVersion = PwVersion {getVersion :: Int}
    deriving newtype (Show, Eq)
