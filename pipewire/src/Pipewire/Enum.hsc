{-# LANGUAGE CPP #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -fno-warn-missing-pattern-synonym-signatures #-}

module Pipewire.Enum where

import Data.Text (Text)
import Foreign (Storable)
import Foreign.C.Types (CInt)
import Pipewire.Protocol

#include <pipewire/pipewire.h>
#include <pipewire/module.h>

newtype PwLinkState = PwLinkState CInt
  deriving newtype (Storable, Show)
  deriving stock (Eq)

-- /* awk '/PW_LINK_STATE/ { print "pattern " $1 " = (#const " $1 ")" }' pipewire/*.h */
pattern PW_LINK_STATE_ERROR = PwLinkState (#const PW_LINK_STATE_ERROR)
pattern PW_LINK_STATE_UNLINKED = PwLinkState (#const PW_LINK_STATE_UNLINKED)
pattern PW_LINK_STATE_INIT = PwLinkState (#const PW_LINK_STATE_INIT)
pattern PW_LINK_STATE_NEGOTIATING = PwLinkState (#const PW_LINK_STATE_NEGOTIATING)
pattern PW_LINK_STATE_ALLOCATING = PwLinkState (#const PW_LINK_STATE_ALLOCATING)
pattern PW_LINK_STATE_PAUSED = PwLinkState (#const PW_LINK_STATE_PAUSED)
pattern PW_LINK_STATE_ACTIVE = PwLinkState (#const PW_LINK_STATE_ACTIVE)

-- /* awk '/#define PW_VERSION_/ { print "m" $2 " :: PwVersion"; print "m" $2 " = PwVersion (#const " $2 ")"; print "" }' pipewire/link.h pipewire/client.h */
mPW_VERSION_LINK :: PwVersion
mPW_VERSION_LINK = PwVersion (#const PW_VERSION_LINK)

mPW_VERSION_LINK_EVENTS :: PwVersion
mPW_VERSION_LINK_EVENTS = PwVersion (#const PW_VERSION_LINK_EVENTS)

mPW_VERSION_LINK_METHODS :: PwVersion
mPW_VERSION_LINK_METHODS = PwVersion (#const PW_VERSION_LINK_METHODS)

mPW_VERSION_CLIENT :: PwVersion
mPW_VERSION_CLIENT = PwVersion (#const PW_VERSION_CLIENT)

mPW_VERSION_CLIENT_EVENTS :: PwVersion
mPW_VERSION_CLIENT_EVENTS = PwVersion (#const PW_VERSION_CLIENT_EVENTS)

mPW_VERSION_CLIENT_METHODS :: PwVersion
mPW_VERSION_CLIENT_METHODS = PwVersion (#const PW_VERSION_CLIENT_METHODS)

newtype PwStreamState = PwStreamState CInt
  deriving newtype (Storable, Show)
  deriving stock (Eq)

#include <pipewire/stream.h>
-- /* awk '/\tPW_STREAM_STATE/ { print "pattern " $1 " = PwStreamState (#const " $1 ") :: PwStreamState" }' pipewire/*.h */
pattern PW_STREAM_STATE_ERROR = PwStreamState (#const PW_STREAM_STATE_ERROR) :: PwStreamState
pattern PW_STREAM_STATE_UNCONNECTED = PwStreamState (#const PW_STREAM_STATE_UNCONNECTED) :: PwStreamState
pattern PW_STREAM_STATE_CONNECTING = PwStreamState (#const PW_STREAM_STATE_CONNECTING) :: PwStreamState
pattern PW_STREAM_STATE_PAUSED = PwStreamState (#const PW_STREAM_STATE_PAUSED) :: PwStreamState
pattern PW_STREAM_STATE_STREAMING = PwStreamState (#const PW_STREAM_STATE_STREAMING) :: PwStreamState

{- | Update with
/* awk '/#define PW_TYPE_INTERFACE_/ { print "m" $2 " :: Text"; print "m" $2 " = " $4; print "" }' pipewire/*.h */
-}
mPW_TYPE_INTERFACE_Client :: Text
mPW_TYPE_INTERFACE_Client = "Client"

mPW_TYPE_INTERFACE_Core :: Text
mPW_TYPE_INTERFACE_Core = "Core"

mPW_TYPE_INTERFACE_Registry :: Text
mPW_TYPE_INTERFACE_Registry = "Registry"

mPW_TYPE_INTERFACE_Device :: Text
mPW_TYPE_INTERFACE_Device = "Device"

mPW_TYPE_INTERFACE_Factory :: Text
mPW_TYPE_INTERFACE_Factory = "Factory"

mPW_TYPE_INTERFACE_Link :: Text
mPW_TYPE_INTERFACE_Link = "Link"

mPW_TYPE_INTERFACE_Module :: Text
mPW_TYPE_INTERFACE_Module = "Module"

mPW_TYPE_INTERFACE_Node :: Text
mPW_TYPE_INTERFACE_Node = "Node"

mPW_TYPE_INTERFACE_Port :: Text
mPW_TYPE_INTERFACE_Port = "Port"
