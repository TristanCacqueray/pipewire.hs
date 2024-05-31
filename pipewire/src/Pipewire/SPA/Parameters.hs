module Pipewire.SPA.Parameters where

import Language.C.Inline qualified as C

import Foreign (allocaBytes)
import Pipewire.Internal
import Pipewire.SPA.CContext

C.context (C.baseCtx <> pwContext)
C.include "<spa/param/video/raw.h>"

newtype SpaVideoInfoRaw = SpaVideoInfoRaw (Ptr SpaVideoInfoRawStruct)

withSpaVideoInfoRaw :: (SpaVideoInfoRaw -> IO a) -> IO a
withSpaVideoInfoRaw cb =
    allocaBytes
        (fromIntegral [C.pure| size_t {sizeof (struct spa_video_info_raw)} |])
        (cb . SpaVideoInfoRaw)
