module Pipewire.Extensions.Metadata where

import Language.C.Inline qualified as C
import Pipewire.CContext
import Pipewire.CoreAPI.Core (Registry (..))
import Pipewire.CoreAPI.Proxy
import Pipewire.Prelude
import Pipewire.Protocol (PwID (..))

C.context (C.baseCtx <> pwContext)

C.include "<pipewire/pipewire.h>"
C.include "<pipewire/extensions/metadata.h>"

newtype Metadata = Metadata {getProxy :: PwProxy}

bindMetadata :: Registry -> PwID -> IO Metadata
bindMetadata (Registry registry) (PwID (fromIntegral -> pwid)) =
    Metadata . PwProxy <$> dieOnNull "pw_registry_bind" do
        [C.exp|struct pw_proxy* {pw_registry_bind(
        $(struct pw_registry* registry)
      , $(uint32_t pwid)
      , PW_TYPE_INTERFACE_Metadata
      , PW_VERSION_METADATA
      , sizeof (struct spa_hook)
  )}|]

pw_metadata_clear :: Metadata -> IO ()
pw_metadata_clear (Metadata (PwProxy pwMetadata)) =
    [C.exp|void{ pw_metadata_clear($(struct pw_proxy* pwMetadata)) }|]

metadataSetProperty :: Metadata -> Word32 -> Text -> Text -> IO ()
metadataSetProperty metadata optID key value =
    withCString key \cOptKey -> withCString value \cOptValue -> do
        pw_metadata_set_property metadata optID cOptKey nullPtr cOptValue

metadataDeleteProperty :: Metadata -> Word32 -> Text -> IO ()
metadataDeleteProperty metadata optID key =
    withCString key \cOptKey -> do
        pw_metadata_set_property metadata optID cOptKey nullPtr nullPtr

pw_metadata_set_property :: Metadata -> Word32 -> CString -> CString -> CString -> IO ()
pw_metadata_set_property (Metadata (PwProxy pwMetadata)) optID optKey optType optValue =
    dieOnErr
        "pw_metadata_set_property"
        [C.exp|int{ pw_metadata_set_property(
          $(struct pw_proxy* pwMetadata)
        , $(uint32_t optID)
        , $(const char* optKey)
        , $(const char* optType)
        , $(const char* optValue)
  ) }|]

withMetadataListener :: MetadataPropertyHandler -> Metadata -> IO a -> IO a
withMetadataListener handler metadata cb =
    withMetadataEvents \mEvents -> withMetadataPropertyHandler handler mEvents do
        addMetadataListener metadata mEvents
        cb

newtype MetadataEvents = MetadataEvents (Ptr PwMetadataEventsStruct)

withMetadataEvents :: (MetadataEvents -> IO a) -> IO a
withMetadataEvents cb =
    allocaBytes
        (fromIntegral [C.pure| size_t {sizeof (struct pw_metadata_events)} |])
        \ptr -> do
            -- TODO: write version number
            cb (MetadataEvents ptr)

type MetadataPropertyHandler = PwID -> MetadataProperty -> IO Int

data MetadataProperty = MetadataProperty
    { key :: Text
    , type_ :: Maybe Text
    , value :: Text
    }
    deriving (Eq, Show)

withMetadataPropertyHandler :: MetadataPropertyHandler -> MetadataEvents -> IO a -> IO a
withMetadataPropertyHandler handler (MetadataEvents metadataEvents) cb = do
    propertyP <- $(C.mkFunPtr [t|Ptr () -> Word32 -> CString -> CString -> CString -> IO CInt|]) wrapper
    [C.block|void{
       struct pw_metadata_events* events = $(struct pw_metadata_events* metadataEvents);
       // TODO: write this when creating the NodeEvents struct
       events->version = PW_VERSION_METADATA_EVENTS;
       events->property = $(int (*propertyP)(void *, uint32_t, const char*, const char*, const char*));
    }|]
    cb `finally` freeHaskellFunPtr propertyP
  where
    wrapper _data (fromIntegral -> subject) mOptKey mOptType mOptValue = do
        key <- peekCString mOptKey
        type_ <- maybePeekCString mOptType
        value <- peekCString mOptValue
        fromIntegral <$> handler (PwID subject) (MetadataProperty{key, type_, value})

addMetadataListener :: Metadata -> MetadataEvents -> IO ()
addMetadataListener (Metadata (PwProxy pwMetadata)) (MetadataEvents metadataEvents) =
    [C.block|void{
    struct pw_proxy* proxy = $(struct pw_proxy* pwMetadata);
    struct spa_hook* hooks = pw_proxy_get_user_data(proxy);

    pw_metadata_add_listener(proxy, hooks, $(struct pw_metadata_events* metadataEvents), NULL);
  }|]
