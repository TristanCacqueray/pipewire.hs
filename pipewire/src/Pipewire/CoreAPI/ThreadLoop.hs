{- | The threaded loop enable using the binding from multiple thread.

Actions needs to be wrapped using 'withLock', and to let the loop run, for example
to take a MVar, then use 'withUnlock'.

TODO: deprecate this API and provide a channel by managing the OsThread ourself with:
- https://gitlab.freedesktop.org/pipewire/pipewire-rs/-/blob/main/pipewire/src/channel.rs
- https://github.com/dimtpap/coppwr/blob/6501b0d0c67d3eb4f572741c5fd35c6279a45803/src/backend/pipewire.rs#L30
-}
module Pipewire.CoreAPI.ThreadLoop where

import Language.C.Inline qualified as C

import Control.Concurrent (myThreadId)
import Control.Exception (bracket_)
import Pipewire.CContext
import Pipewire.CoreAPI.Loop (Loop (..), SignalHandlerRaw)
import Pipewire.Prelude

C.context (C.baseCtx <> pwContext)
C.include "<pipewire/thread-loop.h>"

newtype ThreadLoop = ThreadLoop (Ptr PwThreadLoopStruct)

thread_debug :: Bool
thread_debug = True

threadLog :: String -> IO ()
threadLog name = when thread_debug do
    threadID <- myThreadId
    putStrLn $ "[D] " <> show threadID <> " calling " <> name <> "..."

pw_thread_loop_new :: CString -> IO ThreadLoop
pw_thread_loop_new name = do
    threadLog "pw_thread_loop_new"
    ThreadLoop
        <$> dieOnNull
            "pw_thread_loop_new"
            [C.exp| struct pw_thread_loop*{pw_thread_loop_new($(const char* name), NULL)} |]

pw_thread_loop_lock :: ThreadLoop -> IO ()
pw_thread_loop_lock (ThreadLoop threadLoop) = do
    threadLog "pw_thread_loop_lock"
    -- TODO: fix upstream so that lock returns the do_lock result so that we can die on error
    [C.exp| void{pw_thread_loop_lock($(struct pw_thread_loop* threadLoop))} |]

pw_thread_loop_unlock :: ThreadLoop -> IO ()
pw_thread_loop_unlock (ThreadLoop threadLoop) = do
    threadLog "pw_thread_loop_unlock"
    [C.exp| void{pw_thread_loop_unlock($(struct pw_thread_loop* threadLoop))} |]

-- | A lock must be held when using client implementation.
withLock :: ThreadLoop -> IO a -> IO a
withLock threadLoop = bracket_ (pw_thread_loop_lock threadLoop) (pw_thread_loop_unlock threadLoop)

-- | Temporarly release a lock to let the loop run.
withUnlock :: ThreadLoop -> IO a -> IO a
withUnlock threadLoop = bracket_ (pw_thread_loop_unlock threadLoop) (pw_thread_loop_lock threadLoop)

pw_thread_loop_get_loop :: ThreadLoop -> IO Loop
pw_thread_loop_get_loop (ThreadLoop threadLoop) =
    Loop <$> [C.exp| struct pw_loop*{pw_thread_loop_get_loop($(struct pw_thread_loop* threadLoop))} |]

pw_thread_loop_destroy :: ThreadLoop -> IO ()
pw_thread_loop_destroy (ThreadLoop threadLoop) = do
    threadLog "pw_thread_loop_destroy"
    [C.exp| void{pw_thread_loop_destroy($(struct pw_thread_loop* threadLoop))} |]

pw_thread_loop_wait :: ThreadLoop -> IO ()
pw_thread_loop_wait (ThreadLoop threadLoop) = do
    threadLog "pw_thread_loop_wait"
    [C.exp| void{pw_thread_loop_wait($(struct pw_thread_loop* threadLoop))} |]

pw_thread_loop_start :: ThreadLoop -> IO ()
pw_thread_loop_start (ThreadLoop threadLoop) = do
    threadLog "pw_thread_loop_start"
    dieOnErr "pw_thread_loop_run" [C.exp| int{pw_thread_loop_start($(struct pw_thread_loop* threadLoop))} |]

pw_thread_loop_stop :: ThreadLoop -> IO ()
pw_thread_loop_stop (ThreadLoop threadLoop) = do
    threadLog "pw_thread_loop_stop"
    [C.exp| void{pw_thread_loop_stop($(struct pw_thread_loop* threadLoop))} |]

pw_thread_loop_signal :: ThreadLoop -> Bool -> IO ()
pw_thread_loop_signal (ThreadLoop threadLoop) (fromBool -> waitForAccept) = do
    threadLog "pw_thread_loop_signal"
    [C.exp| void{pw_thread_loop_signal($(struct pw_thread_loop* threadLoop), $(bool waitForAccept))} |]

-- | Stop the loop on SIGINT or SIGKILL
withThreadSignalsHandler :: ThreadLoop -> IO a -> IO a
withThreadSignalsHandler (ThreadLoop threadLoop) cb = do
    handlerP <- $(C.mkFunPtr [t|SignalHandlerRaw|]) handlerWrapper
    [C.block| void{
        struct pw_loop* loop = pw_thread_loop_get_loop($(struct pw_thread_loop* threadLoop));
        pw_loop_add_signal(loop, SIGINT, $(void (*handlerP)(void*, int sig)), NULL);
        pw_loop_add_signal(loop, SIGTERM, $(void (*handlerP)(void*, int sig)), NULL);
    }|]
    cb
        `finally`
        -- TODO: remove the signal handlers?
        freeHaskellFunPtr handlerP
  where
    handlerWrapper _data _sig = pw_thread_loop_signal (ThreadLoop threadLoop) False
