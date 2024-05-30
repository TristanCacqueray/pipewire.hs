module Pipewire.Protocol where

import Foreign (Storable)

newtype SeqID = SeqID {getID :: Int}
    deriving newtype (Show, Eq, Ord)

newtype PwID = PwID {getID :: Int}
    deriving newtype (Show, Eq, Ord, Storable)

newtype PwVersion = PwVersion {getVersion :: Int}
    deriving newtype (Show, Eq, Ord)
