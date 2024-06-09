module Pipewire.CoreAPI.Context where

import Language.C.Inline qualified as C

import Pipewire.CoreAPI.CContext
import Pipewire.CoreAPI.Core (PwCore (..))
import Pipewire.CoreAPI.Loop (PwLoop (..))
import Pipewire.Prelude

newtype PwContext = PwContext (Ptr PwContextStruct)

C.context (C.baseCtx <> pwContext)
C.include "<pipewire/context.h>"

pw_context_new :: PwLoop -> IO PwContext
pw_context_new (PwLoop loop) =
    PwContext
        <$> dieOnNull
            "pw_context_new"
            [C.exp| struct pw_context*{pw_context_new($(struct pw_loop* loop), NULL, 0)} |]

pw_context_connect :: PwContext -> IO PwCore
pw_context_connect (PwContext ctx) =
    PwCore
        <$> dieOnNull
            "pw_context_connect"
            [C.exp| struct pw_core*{pw_context_connect($(struct pw_context* ctx), NULL, 0)} |]

pw_context_destroy :: PwContext -> IO ()
pw_context_destroy (PwContext ctx) =
    [C.exp| void{pw_context_destroy($(struct pw_context* ctx))} |]
