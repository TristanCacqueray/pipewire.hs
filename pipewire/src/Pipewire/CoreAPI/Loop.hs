module Pipewire.CoreAPI.Loop where

import Control.Exception (finally)
import Foreign.Ptr (freeHaskellFunPtr)
import Language.C.Inline qualified as C
import System.Posix.Signals (Signal)

import Pipewire.CoreAPI.CContext
import Pipewire.Internal

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
