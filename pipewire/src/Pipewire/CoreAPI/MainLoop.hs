module Pipewire.CoreAPI.MainLoop where

import Language.C.Inline qualified as C

import Pipewire.CContext
import Pipewire.CoreAPI.Loop (Loop (..), SignalHandlerRaw)
import Pipewire.Prelude

C.context (C.baseCtx <> pwContext)
C.include "<pipewire/main-loop.h>"

newtype MainLoop = MainLoop (Ptr PwMainLoopStruct)

pw_main_loop_new :: IO MainLoop
pw_main_loop_new = MainLoop <$> dieOnNull "pw_main_loop_new" [C.exp| struct pw_main_loop*{pw_main_loop_new(NULL)} |]

pw_main_loop_get_loop :: MainLoop -> IO Loop
pw_main_loop_get_loop (MainLoop mainLoop) =
    Loop <$> [C.exp| struct pw_loop*{pw_main_loop_get_loop($(struct pw_main_loop* mainLoop))} |]

pw_main_loop_destroy :: MainLoop -> IO ()
pw_main_loop_destroy (MainLoop mainLoop) =
    [C.exp| void{pw_main_loop_destroy($(struct pw_main_loop* mainLoop))} |]

pw_main_loop_run :: MainLoop -> IO ()
pw_main_loop_run (MainLoop mainLoop) =
    dieOnErr "pw_main_loop_run" [C.exp| int{pw_main_loop_run($(struct pw_main_loop* mainLoop))} |]

pw_main_loop_quit :: MainLoop -> IO ()
pw_main_loop_quit (MainLoop mainLoop) =
    -- This seems to always return 0
    void $ [C.exp| int{pw_main_loop_quit($(struct pw_main_loop* mainLoop))} |]

-- | Stop the loop on SIGINT or SIGKILL
withSignalsHandler :: MainLoop -> IO a -> IO a
withSignalsHandler (MainLoop mainLoop) cb = do
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
    handlerWrapper _data _sig = pw_main_loop_quit (MainLoop mainLoop)
