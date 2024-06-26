module Pipewire.CoreAPI.Proxy where

import Language.C.Inline qualified as C

import Pipewire.CContext
import Pipewire.Prelude
import Pipewire.SPA.Utilities.Hooks (SpaHook (..), withSpaHook)

C.context (C.baseCtx <> pwContext)

C.include "<pipewire/proxy.h>"

newtype PwProxy = PwProxy (Ptr PwProxyStruct)

type ProxyDestroyHandler = IO ()
type ProxyRemovedHandler = IO ()
type ProxyErrorHandler = Int -> Text -> IO ()

pw_proxy_destroy :: PwProxy -> IO ()
pw_proxy_destroy (PwProxy pwProxy) =
    [C.exp| void{pw_proxy_destroy($(struct pw_proxy* pwProxy))} |]

withProxyEvents :: PwProxy -> ProxyDestroyHandler -> ProxyRemovedHandler -> ProxyErrorHandler -> IO a -> IO a
withProxyEvents (PwProxy pwProxy) destroyHandler removedHandler errorHandler cb =
    withSpaHook \(SpaHook spaHook) -> allocaBytes
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

newtype ProxiedFuncs = ProxiedFuncs (Ptr ())

pw_proxy_add_object_listener :: PwProxy -> SpaHook -> ProxiedFuncs -> IO ()
pw_proxy_add_object_listener (PwProxy pwProxy) (SpaHook spaHook) (ProxiedFuncs funcs) =
    [C.block| void{
                pw_proxy_add_object_listener(
                  $(struct pw_proxy* pwProxy),
                  $(struct spa_hook* spaHook),
                  $(const void *funcs),
                  NULL);
            }|]

newtype ProxyEvents = ProxyEvents (Ptr PwProxyEventsStruct)

withNodeEvents :: (ProxyEvents -> IO a) -> IO a
withNodeEvents cb = do
    allocaBytes
        (fromIntegral [C.pure| size_t {sizeof (struct pw_proxy_events)} |])
        \ptr -> do
            -- TODO: write version number
            cb (ProxyEvents ptr)

-- Note: the proxy needs to have an allocated spa_hook as user data. See 'Node.bindNode' comment.
addProxyListener :: PwProxy -> ProxyEvents -> IO ()
addProxyListener (PwProxy proxy) (ProxyEvents proxyEvents) =
    [C.block|void{
    struct pw_proxy* proxy = $(struct pw_proxy* proxy);
    struct spa_hook* hooks = pw_proxy_get_user_data(proxy);

    pw_proxy_add_listener(proxy, hooks, $(struct pw_proxy_events* proxyEvents), NULL);
  }|]
