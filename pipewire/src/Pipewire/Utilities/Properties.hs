module Pipewire.Utilities.Properties where

import Language.C.Inline qualified as C

import Foreign (Storable (peek), alloca, nullPtr)
import Pipewire.Internal
import Pipewire.SPA.Utilities.CContext qualified as SPA
import Pipewire.SPA.Utilities.Dictionary (SpaDict (..))
import Pipewire.Utilities.CContext

C.context (C.baseCtx <> pwContext <> SPA.pwContext)

C.include "<pipewire/properties.h>"

newtype PwProperties = PwProperties (Ptr PwPropertiesStruct)

pw_properties_new :: IO PwProperties
pw_properties_new =
    PwProperties <$> [C.exp| struct pw_properties*{pw_properties_new(NULL, NULL)} |]

pw_properties_new_dict :: SpaDict -> IO PwProperties
pw_properties_new_dict (SpaDict spaDict) =
    PwProperties <$> [C.exp| struct pw_properties*{pw_properties_new_dict($(const struct spa_dict* spaDict))} |]

pw_properties_set :: PwProperties -> Text -> Text -> IO ()
pw_properties_set (PwProperties pwProperties) key val =
    withCString key \keyPtr -> do
        withCString val \valPtr -> do
            [C.exp| void{pw_properties_set($(struct pw_properties* pwProperties), $(const char* keyPtr), $(const char* valPtr))}|]

pw_properties_get :: PwProperties -> Text -> IO (Maybe Text)
pw_properties_get (PwProperties pwProperties) key = withCString key \kPtr -> alloca \(cPtr :: Ptr CString) -> do
    [C.block| void{
    const char** result = $(const char** cPtr);
    struct pw_properties *props = $(struct pw_properties* pwProperties);
    *result = pw_properties_get(props, $(const char* kPtr));
  }|]
    result <- peek cPtr
    if result == nullPtr
        then pure Nothing
        else Just <$> peekCString result
