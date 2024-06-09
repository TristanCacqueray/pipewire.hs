module Pipewire.Utilities.Properties where

import Language.C.Inline qualified as C

import Pipewire.Prelude
import Pipewire.Protocol (PwID (..))
import Pipewire.SPA.CContext qualified as SPA
import Pipewire.SPA.Utilities.Dictionary (SpaDict (..))
import Pipewire.Utilities.CContext

C.context (C.baseCtx <> pwContext <> SPA.pwContext)

C.include "<pipewire/properties.h>"
C.include "<pipewire/keys.h>"

newtype PwProperties = PwProperties (Ptr PwPropertiesStruct)

-- | Keep the object after the client disconnect
pw_properties_set_linger :: PwProperties -> IO ()
pw_properties_set_linger (PwProperties pwProperties) =
    [C.exp| void{pw_properties_set($(struct pw_properties* pwProperties), PW_KEY_OBJECT_LINGER, "true")}|]

-- TODO: bind pw_properties_destroy and provide withProperties wrapper
pw_properties_new :: IO PwProperties
pw_properties_new =
    PwProperties
        <$> dieOnNull
            "pw_properties_new"
            [C.exp| struct pw_properties*{pw_properties_new(NULL, NULL)} |]

pw_properties_new_dict :: SpaDict -> IO PwProperties
pw_properties_new_dict (SpaDict spaDict) =
    PwProperties
        <$> dieOnNull
            "pw_properties_new_dict"
            [C.exp| struct pw_properties*{pw_properties_new_dict($(const struct spa_dict* spaDict))} |]

pw_properties_set :: PwProperties -> Text -> Text -> IO ()
pw_properties_set (PwProperties pwProperties) key val =
    withCString key \keyPtr -> do
        withCString val \valPtr -> do
            [C.exp| void{pw_properties_set($(struct pw_properties* pwProperties), $(const char* keyPtr), $(const char* valPtr))}|]

pw_properties_set_id :: PwProperties -> Text -> PwID -> IO ()
pw_properties_set_id (PwProperties pwProperties) key (PwID (fromIntegral -> val)) =
    withCString key \keyPtr -> do
        [C.exp| void{pw_properties_setf($(struct pw_properties* pwProperties), $(const char* keyPtr), "%u", $(int val))}|]

pw_properties_get :: PwProperties -> Text -> IO (Maybe Text)
pw_properties_get (PwProperties pwProperties) key = withCString key \kPtr -> do
    result <-
        [C.block| const char*{
      struct pw_properties *props = $(struct pw_properties* pwProperties);
      return pw_properties_get(props, $(const char* kPtr));
    }|]
    if result == nullPtr
        then pure Nothing
        else Just <$> peekCString result
