module Pipewire.CoreAPI.Loop where

import Language.C.Inline qualified as C
import System.Posix.Signals (Signal)

import Pipewire.CContext
import Pipewire.Prelude

C.context (C.baseCtx <> pwContext)

C.include "<pipewire/loop.h>"

newtype PwLoop = PwLoop (Ptr PwLoopStruct)

type SignalHandlerRaw = Ptr () -> Signal -> IO ()

pw_loop_add_signal :: PwLoop -> Signal -> (Signal -> IO ()) -> IO a -> IO a
pw_loop_add_signal (PwLoop pwLoop) signal handler cb = do
    handlerP <- $(C.mkFunPtr [t|SignalHandlerRaw|]) handlerWrapper
    [C.exp| void{pw_loop_add_signal($(struct pw_loop* pwLoop), $(int signal), $(void (*handlerP)(void*, int sig)), NULL)} |]
    cb
        `finally` freeHaskellFunPtr handlerP
  where
    handlerWrapper _data = handler

type TimerHandler = Word64 -> IO ()

newtype SpaSource = SpaSource (Ptr SpaSourceStruct)

pw_loop_add_timer :: PwLoop -> TimerHandler -> (SpaSource -> IO a) -> IO a
pw_loop_add_timer (PwLoop pwLoop) handler cb = do
    handlerP <- $(C.mkFunPtr [t|Ptr () -> Word64 -> IO ()|]) handlerWrapper
    timer <-
        SpaSource
            <$> [C.exp| struct spa_source*{pw_loop_add_timer(
                    $(struct pw_loop* pwLoop),
                    $(void (*handlerP)(void*,uint64_t)),
                    NULL
              )} |]
    cb timer `finally` do
        freeHaskellFunPtr handlerP
  where
    handlerWrapper _userData = handler
