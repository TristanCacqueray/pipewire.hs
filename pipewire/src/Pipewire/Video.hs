{- | This integrates the video-src.c examples, this is just a place holder to try this out.
SPDX-FileCopyrightText: Copyright © 2018 Wim Taymans
SPDX-License-Identifier: MIT
-}
module Pipewire.Video where

import Foreign (castPtr)
import Language.C.Inline qualified as C

import Pipewire.CContext
import Pipewire.CoreAPI.Core (PwCore)
import Pipewire.CoreAPI.Loop (SpaSource (..), pw_loop_add_timer)
import Pipewire.CoreAPI.MainLoop (PwMainLoop (..), pw_main_loop_get_loop)
import Pipewire.Enum
import Pipewire.Prelude
import Pipewire.SPA.POD (SpaPod (..))
import Pipewire.SPA.Parameters
import Pipewire.SPA.Utilities.Hooks (SpaHook (..), withSpaHook)
import Pipewire.Stream
import Pipewire.Utilities.Properties (PwProperties (..))

C.context (C.baseCtx <> C.vecCtx <> pwContext)

C.include "<math.h>"
C.include "<spa/param/video/format-utils.h>"
C.include "<spa/param/tag-utils.h>"
C.include "<spa/debug/pod.h>"
C.include "<spa/debug/buffer.h>"
C.include "<pipewire/pipewire.h>"

bpp, maxBuffer :: CInt
bpp = 4
maxBuffer = 64

type StateHandler = PwStream -> PwStreamState -> IO ()
type DrawHandler = Ptr Word32 -> CInt -> CInt -> IO ()

onProcess :: FunPtr DrawHandler -> PwStream -> StridePtr -> SeqPtr -> DoublePtr -> DoublePtr -> SpaVideoInfoRaw -> IO ()
onProcess draw (PwStream pwStream) (StridePtr stride) (SeqPtr seq') (DoublePtr crop) (DoublePtr acc) (SpaVideoInfoRaw videoInfo) =
    [C.block| void{
        struct spa_video_info_raw* format = $(struct spa_video_info_raw* videoInfo);
        uint32_t* stride = $(uint32_t* stride);
        uint32_t* seq = $(uint32_t* seq');
        struct pw_stream *stream = $(struct pw_stream* pwStream);
        double* crop = $(double* crop);
        double* accumulator = $(double* acc);

        struct pw_buffer *b;
        struct spa_buffer *buf;
        uint32_t *pixels;
        if ((b = pw_stream_dequeue_buffer(stream)) == NULL) {
                pw_log_warn("out of buffers: %m");
                return;
        }

        buf = b->buffer;
        if ((pixels = buf->datas[0].data) == NULL)
                return;

        struct spa_meta *m;
        struct spa_meta_header *h;
        struct spa_meta_region *mc;

        if ((h = spa_buffer_find_meta_data(buf, SPA_META_Header, sizeof(*h)))) {
#if 0
                h->pts = pw_stream_get_nsec(data->stream);
#else
                h->pts = -1;
#endif
                h->flags = 0;
                h->seq = *seq++;
                h->dts_offset = 0;
        }
        if ((m = spa_buffer_find_meta(buf, SPA_META_VideoDamage))) {
                struct spa_meta_region *r = spa_meta_first(m);

                if (spa_meta_check(r, m)) {
                        r->region.position = SPA_POINT(0,0);
                        r->region.size = format->size;
                        r++;
                }
                if (spa_meta_check(r, m))
                        r->region = SPA_REGION(0,0,0,0);
        }
        if ((mc = spa_buffer_find_meta_data(buf, SPA_META_VideoCrop, sizeof(*mc)))) {
                *crop = (sin(*accumulator) + 1.0) * 32.0;
                mc->region.position.x = *crop;
                mc->region.position.y = *crop;
                mc->region.size.width = format->size.width - *crop*2;
                mc->region.size.height = format->size.height - *crop*2;
        }

        $(void (*draw)(uint32_t*,int,int))(pixels, format->size.width, format->size.height);

        #define M_PI_M2 ( M_PI + M_PI )
        *accumulator += M_PI_M2 / 50.0;
        if (*accumulator >= M_PI_M2)
                *accumulator -= M_PI_M2;

        buf->datas[0].chunk->offset = 0;
        buf->datas[0].chunk->size = format->size.height * *stride;
        buf->datas[0].chunk->stride = *stride;
        pw_stream_queue_buffer(stream, b);
  }|]

newtype DoublePtr = DoublePtr (Ptr C.CDouble)
newtype SeqPtr = SeqPtr (Ptr Word32)
newtype StridePtr = StridePtr (Ptr Word32)

withDoublePtr :: (DoublePtr -> IO a) -> IO a
withDoublePtr cb = alloca (cb . DoublePtr)
withSeqPtr :: (SeqPtr -> IO a) -> IO a
withSeqPtr cb = alloca (cb . SeqPtr)
withStridePtr :: (StridePtr -> IO a) -> IO a
withStridePtr cb = alloca (cb . StridePtr)

onStreamParamChanged :: PwStream -> StridePtr -> SpaVideoInfoRaw -> Word32 -> SpaPod -> IO ()
onStreamParamChanged (PwStream pwStream) (StridePtr stride) (SpaVideoInfoRaw videoInfo) id' (SpaPod spaPod) =
    [C.block| void{
        struct spa_video_info_raw* format = $(struct spa_video_info_raw* videoInfo);
        uint32_t* stride = $(uint32_t* stride);
        uint32_t id = $(uint32_t id');
        const struct spa_pod *param = $(struct spa_pod* spaPod);
        struct pw_stream *stream = $(struct pw_stream* pwStream);
        uint8_t params_buffer[1024];
        struct spa_pod_builder b = SPA_POD_BUILDER_INIT(params_buffer, sizeof(params_buffer));
        const struct spa_pod *params[4];

        if (param != NULL && id == SPA_PARAM_Tag) {
                spa_debug_pod(0, NULL, param);
                return;
        }
        if (param == NULL || id != SPA_PARAM_Format)
                return;

        spa_format_video_raw_parse(param, format);

        *stride = SPA_ROUND_UP_N(format->size.width * $(int bpp), 4);
        printf("stride: %d\n", *stride);

        params[0] = spa_pod_builder_add_object(&b,
                SPA_TYPE_OBJECT_ParamBuffers, SPA_PARAM_Buffers,
                SPA_PARAM_BUFFERS_buffers, SPA_POD_CHOICE_RANGE_Int(8, 2, $(int maxBuffer)),
                SPA_PARAM_BUFFERS_blocks,  SPA_POD_Int(1),
                SPA_PARAM_BUFFERS_size,    SPA_POD_Int(*stride * format->size.height),
                SPA_PARAM_BUFFERS_stride,  SPA_POD_Int(*stride));

        params[1] = spa_pod_builder_add_object(&b,
                SPA_TYPE_OBJECT_ParamMeta, SPA_PARAM_Meta,
                SPA_PARAM_META_type, SPA_POD_Id(SPA_META_Header),
                SPA_PARAM_META_size, SPA_POD_Int(sizeof(struct spa_meta_header)));

        params[2] = spa_pod_builder_add_object(&b,
                SPA_TYPE_OBJECT_ParamMeta, SPA_PARAM_Meta,
                SPA_PARAM_META_type, SPA_POD_Id(SPA_META_VideoDamage),
                SPA_PARAM_META_size, SPA_POD_CHOICE_RANGE_Int(
                                        sizeof(struct spa_meta_region) * 16,
                                        sizeof(struct spa_meta_region) * 1,
                                        sizeof(struct spa_meta_region) * 16));
        params[3] = spa_pod_builder_add_object(&b,
                SPA_TYPE_OBJECT_ParamMeta, SPA_PARAM_Meta,
                SPA_PARAM_META_type, SPA_POD_Id(SPA_META_VideoCrop),
                SPA_PARAM_META_size, SPA_POD_Int(sizeof(struct spa_meta_region)));

        pw_stream_update_params(stream, params, 4);
  }|]

onStreamStateChanged :: PwMainLoop -> SpaSource -> PwStream -> PwStreamState -> PwStreamState -> IO ()
onStreamStateChanged (PwMainLoop mainLoop) (SpaSource timer) (PwStream pwStream) (PwStreamState _old) (PwStreamState state) =
    [C.block| void{
        struct pw_main_loop* loop = $(struct pw_main_loop* mainLoop);
        enum pw_stream_state state = $(int state);
        struct spa_source *timer = $(struct spa_source* timer);
        struct pw_stream *stream = $(struct pw_stream* pwStream);

        printf("stream state: \"%s\"\n", pw_stream_state_as_string(state));

        switch (state) {
        case PW_STREAM_STATE_ERROR:
        case PW_STREAM_STATE_UNCONNECTED:
                pw_main_loop_quit(loop);
                break;

        case PW_STREAM_STATE_PAUSED:
                pw_loop_update_timer(pw_main_loop_get_loop(loop),
                                timer, NULL, NULL, false);
                break;
        case PW_STREAM_STATE_STREAMING:
        {
                struct timespec timeout, interval;

                timeout.tv_sec = 0;
                timeout.tv_nsec = 1;
                interval.tv_sec = 0;
                interval.tv_nsec = 40 * SPA_NSEC_PER_MSEC;

                pw_loop_update_timer(pw_main_loop_get_loop(loop),
                                timer, &timeout, &interval, false);
                break;
        }
        default:
                break;
        }
  }|]

withVideoHandlers :: PwMainLoop -> PwStream -> StateHandler -> DrawHandler -> (PwStream -> IO a) -> IO a
withVideoHandlers pwMainLoop pwStream@(PwStream s) stateHandler draw cb =
    withSpaHook \(SpaHook spaHook) -> withStridePtr \stridePtr -> do
        loop <- pw_main_loop_get_loop pwMainLoop
        pw_loop_add_timer loop onTimeoutHandler \timer ->
            withSeqPtr \seqPtr -> withDoublePtr \cropPtr -> withDoublePtr \accPtr -> withSpaVideoInfoRaw \videoInfo -> do
                withStreamEvents \(PwStreamEvents streamEvents) -> do
                    drawHandler <- $(C.mkFunPtr [t|DrawHandler|]) draw
                    let
                        onProcessWrapper _data =
                            onProcess drawHandler pwStream stridePtr seqPtr cropPtr accPtr videoInfo
                    onProcessP <- $(C.mkFunPtr [t|Ptr () -> IO ()|]) onProcessWrapper
                    let
                        onStateChangedWrapper _data (PwStreamState -> old) (PwStreamState -> state) _error = do
                            onStreamStateChanged pwMainLoop timer pwStream old state
                            stateHandler pwStream state
                        onParamChangedWrapper _data id' param =
                            onStreamParamChanged pwStream stridePtr videoInfo id' (SpaPod (castPtr param))
                        onTriggerDoneWrapper _data =
                            [C.exp|void{pw_log_trace("trigger done")}|]
                    onStateChanged <- $(C.mkFunPtr [t|Ptr () -> CInt -> CInt -> CString -> IO ()|]) onStateChangedWrapper
                    onParamChanged <- $(C.mkFunPtr [t|Ptr () -> Word32 -> Ptr SpaPodStruct -> IO ()|]) onParamChangedWrapper
                    onTriggerDone <- $(C.mkFunPtr [t|Ptr () -> IO ()|]) onTriggerDoneWrapper
                    [C.block| void{
                struct pw_stream_events* pw_events = $(struct pw_stream_events* streamEvents);
                pw_events->version = PW_VERSION_STREAM_EVENTS;
                pw_events->process = $(void (*onProcessP)(void*));
                pw_events->state_changed = $(void (*onStateChanged)(void*,int,int,const char*));
                pw_events->param_changed = $(void (*onParamChanged)(void*,uint32_t,const struct spa_pod*));
                pw_events->trigger_done = $(void (*onTriggerDone)(void*));


                pw_stream_add_listener($(struct pw_stream* s),
                    $(struct spa_hook* spaHook),
                    pw_events,
                    NULL);
            }|]
                    cb pwStream `finally` do
                        freeHaskellFunPtr onProcessP
                        freeHaskellFunPtr onStateChanged
                        freeHaskellFunPtr onParamChanged
                        freeHaskellFunPtr onTriggerDone
                        freeHaskellFunPtr drawHandler
  where
    onTimeoutHandler _expirations = do
        -- putStrLn "onTimeout"
        pw_stream_trigger_process pwStream

withVideoStream :: PwMainLoop -> PwCore -> StateHandler -> DrawHandler -> (PwStream -> IO a) -> IO a
withVideoStream pwMainLoop pwCore stateHandler draw cb = do
    props <-
        PwProperties
            <$> dieOnNull
                "pw_properties_new"
                [C.exp| struct pw_properties*{
                 pw_properties_new(PW_KEY_MEDIA_CLASS, "Video/Source", NULL)
           }|]
    pwStream <- pw_stream_new pwCore "video-src" props
    withVideoHandlers pwMainLoop pwStream stateHandler draw cb

connectVideoStream :: PwStream -> IO ()
connectVideoStream (PwStream pwStream) =
    [C.block| void{
        const struct spa_pod *params[2];
        uint8_t buffer[1024];
        struct spa_pod_builder b = SPA_POD_BUILDER_INIT(buffer, sizeof(buffer));

        params[0] = spa_pod_builder_add_object(&b,
                SPA_TYPE_OBJECT_Format, SPA_PARAM_EnumFormat,
                SPA_FORMAT_mediaType,       SPA_POD_Id(SPA_MEDIA_TYPE_video),
                SPA_FORMAT_mediaSubtype,    SPA_POD_Id(SPA_MEDIA_SUBTYPE_raw),
                SPA_FORMAT_VIDEO_format,    SPA_POD_Id(SPA_VIDEO_FORMAT_RGBA),
                SPA_FORMAT_VIDEO_size,      SPA_POD_CHOICE_RANGE_Rectangle(
                                                &SPA_RECTANGLE(320, 240),
                                                &SPA_RECTANGLE(1, 1),
                                                &SPA_RECTANGLE(4096, 4096)),
                SPA_FORMAT_VIDEO_framerate, SPA_POD_Fraction(&SPA_FRACTION(25, 1)));

        {
                struct spa_pod_frame f;
                struct spa_dict_item items[1];
                /* send a tag, output tags travel downstream */
                spa_tag_build_start(&b, &f, SPA_PARAM_Tag, SPA_DIRECTION_OUTPUT);
                items[0] = SPA_DICT_ITEM_INIT("my-tag-key", "my-special-tag-value");
                spa_tag_build_add_dict(&b, &SPA_DICT_INIT(items, 1));
                params[1] = spa_tag_build_end(&b, &f);
        }

        pw_stream_connect($(struct pw_stream* pwStream),
                          PW_DIRECTION_OUTPUT,
                          PW_ID_ANY,
                          PW_STREAM_FLAG_DRIVER |
                          PW_STREAM_FLAG_MAP_BUFFERS,
                          params, 2);
  }|]
