module Pipewire.CoreAPI.Initialization where

import Language.C.Inline qualified as C

C.include "<pipewire/pipewire.h>"

-- Q: do we need to pass the real argc/argv ?
pw_init :: IO ()
pw_init = [C.exp| void{pw_init(NULL, NULL)} |]

pw_deinit :: IO ()
pw_deinit = [C.exp| void{pw_deinit()} |]
