module Pipewire.SPA.Utilities.Hooks where

import Language.C.Inline qualified as C

import Pipewire.Prelude
import Pipewire.SPA.CContext

C.context (C.baseCtx <> pwContext)

C.include "<spa/utils/hook.h>"

newtype SpaHook = SpaHook (Ptr SpaHookStruct)

-- | Create a local spa_hook structure
with_spa_hook :: (SpaHook -> IO a) -> IO a
with_spa_hook cb = allocaBytes
    (fromIntegral size)
    \p -> do
        -- Do we need to memset after allocaBytes ?
        [C.exp| void{spa_memzero($(struct spa_hook* p), $(size_t size))} |]
        cb (SpaHook p)
  where
    size = [C.pure| size_t {sizeof (struct spa_hook)} |]
