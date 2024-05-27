module Pipewire.SPA.Utilities.Hooks where

import Foreign (Ptr)
import Language.C.Inline qualified as C

import Pipewire.SPA.Utilities.CContext

C.context (C.baseCtx <> pwContext)

C.include "<spa/utils/hook.h>"

newtype SpaHook = SpaHook (Ptr SpaHookStruct)
