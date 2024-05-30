module Pipewire.CoreAPI.Proxy where

import Control.Exception (finally)
import Foreign (allocaBytes, freeHaskellFunPtr)
import Language.C.Inline qualified as C

import Pipewire.CoreAPI.CContext
import Pipewire.Internal
import Pipewire.SPA.Utilities.CContext qualified as SPAUtils
import Pipewire.SPA.Utilities.Hooks (SpaHook (..), with_spa_hook)

C.context (C.baseCtx <> pwContext <> SPAUtils.pwContext)

C.include "<pipewire/proxy.h>"

newtype PwProxy = PwProxy (Ptr PwProxyStruct)

type ProxyDestroyHandler = IO ()
type ProxyRemovedHandler = IO ()
type ProxyErrorHandler = Int -> Text -> IO ()

pw_proxy_destroy :: PwProxy -> IO ()
pw_proxy_destroy (PwProxy pwProxy) =
    [C.exp| void{pw_proxy_destroy($(struct pw_proxy* pwProxy))} |]

with_pw_proxy_events :: PwProxy -> ProxyDestroyHandler -> ProxyRemovedHandler -> ProxyErrorHandler -> IO a -> IO a
with_pw_proxy_events (PwProxy pwProxy) destroyHandler removedHandler errorHandler cb =
    with_spa_hook \(SpaHook spaHook) -> allocaBytes
        (fromIntegral [C.pure| size_t {sizeof (struct pw_proxy_events)} |])
        \p -> do
            destroyP <- $(C.mkFunPtr [t|Ptr () -> IO ()|]) (const destroyHandler)
            removedP <- $(C.mkFunPtr [t|Ptr () -> IO ()|]) (const removedHandler)
            errorP <- $(C.mkFunPtr [t|Ptr () -> CInt -> CInt -> CString -> IO ()|]) errorWrapper
            [C.block| void{
                struct pw_proxy_events* pe = $(struct pw_proxy_events* p);
                pe->version = PW_VERSION_PROXY_EVENTS;
                pe->destroy = $(void (*destroyP)(void*));
                pe->removed = $(void (*removedP)(void*));
                pe->error = $(void (*errorP)(void*, int seq, int res, const char* message));
                pw_proxy_add_listener(
                  $(struct pw_proxy* pwProxy),
                  $(struct spa_hook* spaHook),
                  pe,
                  NULL);
        }|]
            cb
                `finally` do
                    [C.exp| void{spa_hook_remove($(struct spa_hook* spaHook))}|]
                    freeHaskellFunPtr destroyP
                    freeHaskellFunPtr removedP
                    freeHaskellFunPtr errorP
  where
    errorWrapper _data _seq res message = do
        err <- peekCString message
        errorHandler (fromIntegral res) err
