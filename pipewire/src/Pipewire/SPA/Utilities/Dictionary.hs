module Pipewire.SPA.Utilities.Dictionary where

import Data.Text.Read qualified as Text
import Data.Vector qualified as V
import Data.Vector.Storable.Mutable qualified as VM
import Language.C.Inline qualified as C

import Pipewire.Prelude
import Pipewire.SPA.CContext

newtype SpaDict = SpaDict (Ptr SpaDictStruct)

C.context (C.baseCtx <> C.vecCtx <> pwContext)
C.include "<spa/utils/dict.h>"

spaDictLookup :: SpaDict -> Text -> IO (Maybe Text)
spaDictLookup (SpaDict spaDict) key = withCString key \kPtr -> do
    result <-
        [C.block| const char*{
      struct spa_dict *spa_dict = $(struct spa_dict* spaDict);
      return spa_dict_lookup(spa_dict, $(const char* kPtr));
    }|]
    if result == nullPtr
        then pure Nothing
        else Just <$> peekCString result

spaDictLookupInt :: SpaDict -> Text -> IO (Maybe Int)
spaDictLookupInt dict key = readInt <$> spaDictLookup dict key
  where
    readInt = \case
        Nothing -> Nothing
        Just txt -> case Text.decimal txt of
            Right (n, "") -> pure n
            _ -> Nothing

-- | Read 'SpaDict' keys/values
spaDictRead :: SpaDict -> IO (V.Vector (Text, Text))
spaDictRead (SpaDict spaDict) = do
    -- Read the dictionary size
    propSize <-
        fromIntegral
            <$> [C.exp| int{$(struct spa_dict* spaDict)->n_items}|]

    -- Create two mutable vectors to store the key and value char* pointer.
    vecKey <- VM.new propSize
    vecValue <- VM.new propSize

    -- Fill the vectors
    [C.block| void {
      // The Haskell spaDict
      struct spa_dict *spa_dict = $(struct spa_dict* spaDict);

      // The Haskell vectors
      const char** keys = $vec-ptr:(const char **vecKey);
      const char** values = $vec-ptr:(const char **vecValue);

      // Use the provided 'spa_dict_for_each' macro to traverse the c struct
      const struct spa_dict_item *item;
      int item_pos = 0;
      spa_dict_for_each(item, spa_dict) {
        keys[item_pos] = item->key;
        values[item_pos] = item->value;
        item_pos += 1;
      };
    }|]

    -- Convert the CString vectors into a vector of Text (key,value) tuples
    let readCString pos = do
            key <- peekCString =<< VM.read vecKey pos
            val <- peekCString =<< VM.read vecValue pos
            pure (key, val)
    V.generateM propSize readCString

-- | Create a local spa_hook structure
with_spa_dict :: (SpaDict -> IO a) -> IO a
with_spa_dict cb = allocaBytes
    (fromIntegral size)
    \p -> do
        -- Do we need to memset after allocaBytes ?
        [C.exp| void{spa_memzero($(struct spa_dict* p), $(size_t size))} |]
        cb (SpaDict p)
  where
    size = [C.pure| size_t {sizeof (struct spa_dict)} |]
