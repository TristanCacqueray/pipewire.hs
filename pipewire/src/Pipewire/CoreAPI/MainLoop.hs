module Pipewire.CoreAPI.MainLoop where

import Language.C.Inline qualified as C

import Pipewire.CoreAPI.CContext
import Pipewire.CoreAPI.Loop (PwLoop (..))
import Pipewire.Internal

newtype PwContext = PwContext (Ptr PwContextStruct)

C.context (C.baseCtx <> pwContext)
C.include "<pipewire/main-loop.h>"

newtype PwMainLoop = PwMainLoop (Ptr PwMainLoopStruct)

pw_main_loop_new :: IO PwMainLoop
pw_main_loop_new = PwMainLoop <$> [C.exp| struct pw_main_loop*{pw_main_loop_new(NULL)} |]

pw_main_loop_get_loop :: PwMainLoop -> IO PwLoop
pw_main_loop_get_loop (PwMainLoop mainLoop) =
    PwLoop <$> [C.exp| struct pw_loop*{pw_main_loop_get_loop($(struct pw_main_loop* mainLoop))} |]

pw_main_loop_destroy :: PwMainLoop -> IO ()
pw_main_loop_destroy (PwMainLoop mainLoop) =
    [C.exp| void{pw_main_loop_destroy($(struct pw_main_loop* mainLoop))} |]

pw_main_loop_run :: PwMainLoop -> IO CInt
pw_main_loop_run (PwMainLoop mainLoop) =
    [C.exp| int{pw_main_loop_run($(struct pw_main_loop* mainLoop))} |]

pw_main_loop_quit :: PwMainLoop -> IO CInt
pw_main_loop_quit (PwMainLoop mainLoop) =
    [C.exp| int{pw_main_loop_quit($(struct pw_main_loop* mainLoop))} |]
