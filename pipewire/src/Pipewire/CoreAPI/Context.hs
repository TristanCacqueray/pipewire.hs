module Pipewire.CoreAPI.Context where

import Language.C.Inline qualified as C

import Pipewire.CContext
import Pipewire.CoreAPI.Core (Core (..))
import Pipewire.CoreAPI.Loop (Loop (..))
import Pipewire.Prelude

newtype Context = Context (Ptr PwContextStruct)

C.context (C.baseCtx <> pwContext)
C.include "<pipewire/context.h>"

pw_context_new :: Loop -> IO Context
pw_context_new (Loop loop) =
    Context
        <$> dieOnNull
            "pw_context_new"
            [C.exp| struct pw_context*{pw_context_new($(struct pw_loop* loop), NULL, 0)} |]

pw_context_connect :: Context -> IO Core
pw_context_connect (Context ctx) =
    Core
        <$> dieOnNull
            "pw_context_connect"
            [C.exp| struct pw_core*{pw_context_connect($(struct pw_context* ctx), NULL, 0)} |]

pw_context_destroy :: Context -> IO ()
pw_context_destroy (Context ctx) =
    [C.exp| void{pw_context_destroy($(struct pw_context* ctx))} |]
