{-# LANGUAGE CPP #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -fno-warn-missing-pattern-synonym-signatures #-}

module Pipewire.Enum where

import Foreign (Storable)
import Foreign.C.Types (CInt)
import Pipewire.Protocol

#include <pipewire/pipewire.h>
#include <pipewire/module.h>

newtype PwLinkState = PwLinkState CInt
  deriving newtype (Storable, Show)
  deriving stock (Eq)

-- /* awk '/PW_LINK_STATE/ { print "pattern " $1 " = (#const " $1 ") :: PwLinkState" }' pipewire/*.h */
pattern PW_LINK_STATE_ERROR = PwLinkState (#const PW_LINK_STATE_ERROR) :: PwLinkState
pattern PW_LINK_STATE_UNLINKED = PwLinkState (#const PW_LINK_STATE_UNLINKED) :: PwLinkState
pattern PW_LINK_STATE_INIT = PwLinkState (#const PW_LINK_STATE_INIT) :: PwLinkState
pattern PW_LINK_STATE_NEGOTIATING = PwLinkState (#const PW_LINK_STATE_NEGOTIATING) :: PwLinkState
pattern PW_LINK_STATE_ALLOCATING = PwLinkState (#const PW_LINK_STATE_ALLOCATING) :: PwLinkState
pattern PW_LINK_STATE_PAUSED = PwLinkState (#const PW_LINK_STATE_PAUSED) :: PwLinkState
pattern PW_LINK_STATE_ACTIVE = PwLinkState (#const PW_LINK_STATE_ACTIVE) :: PwLinkState

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
