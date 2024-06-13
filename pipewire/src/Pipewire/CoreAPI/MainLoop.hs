module Pipewire.CoreAPI.MainLoop where

import Language.C.Inline qualified as C

import Pipewire.CContext
import Pipewire.CoreAPI.Loop (PwLoop (..), SignalHandlerRaw)
import Pipewire.Prelude

newtype PwContext = PwContext (Ptr PwContextStruct)

C.context (C.baseCtx <> pwContext)
C.include "<pipewire/main-loop.h>"

newtype PwMainLoop = PwMainLoop (Ptr PwMainLoopStruct)

pw_main_loop_new :: IO PwMainLoop
pw_main_loop_new = PwMainLoop <$> dieOnNull "pw_main_loop_new" [C.exp| struct pw_main_loop*{pw_main_loop_new(NULL)} |]

pw_main_loop_get_loop :: PwMainLoop -> IO PwLoop
pw_main_loop_get_loop (PwMainLoop mainLoop) =
    PwLoop <$> [C.exp| struct pw_loop*{pw_main_loop_get_loop($(struct pw_main_loop* mainLoop))} |]

pw_main_loop_destroy :: PwMainLoop -> IO ()
pw_main_loop_destroy (PwMainLoop mainLoop) =
    [C.exp| void{pw_main_loop_destroy($(struct pw_main_loop* mainLoop))} |]

pw_main_loop_run :: PwMainLoop -> IO ()
pw_main_loop_run (PwMainLoop mainLoop) =
    dieOnErr "pw_main_loop_run" [C.exp| int{pw_main_loop_run($(struct pw_main_loop* mainLoop))} |]

pw_main_loop_quit :: PwMainLoop -> IO ()
pw_main_loop_quit (PwMainLoop mainLoop) =
    -- This seems to always return 0
    void $ [C.exp| int{pw_main_loop_quit($(struct pw_main_loop* mainLoop))} |]

-- | Stop the loop on SIGINT or SIGKILL
withSignalsHandler :: PwMainLoop -> IO a -> IO a
withSignalsHandler (PwMainLoop mainLoop) cb = do
    handlerP <- $(C.mkFunPtr [t|SignalHandlerRaw|]) handlerWrapper
    [C.block| void{
        struct pw_loop* loop = pw_main_loop_get_loop($(struct pw_main_loop* mainLoop));
        pw_loop_add_signal(loop, SIGINT, $(void (*handlerP)(void*, int sig)), NULL);
        pw_loop_add_signal(loop, SIGTERM, $(void (*handlerP)(void*, int sig)), NULL);
    }|]
    cb
        `finally`
        -- TODO: remove the signal handlers?
        freeHaskellFunPtr handlerP
  where
    handlerWrapper _data _sig = pw_main_loop_quit (PwMainLoop mainLoop)
