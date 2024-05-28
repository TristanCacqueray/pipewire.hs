module Pipewire.Constants where

import Data.Text

{- | Update with
awk '/#define PW_TYPE_INTERFACE_/ { print "m" $2 " :: Text"; print "m" $2 " = " $4; print "" }' pipewire/*.h
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
