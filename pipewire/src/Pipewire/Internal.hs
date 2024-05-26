-- | Helper functions
module Pipewire.Internal where

import Data.Text (Text, pack)
import Foreign.C.String (CString)
import GHC.Foreign qualified as Foreign
import System.IO (utf8)

peekCString :: CString -> IO Text
peekCString = fmap pack . Foreign.peekCString utf8
