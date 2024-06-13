module Pipewire.Stream where

import Language.C.Inline qualified as C

import Data.Vector.Storable qualified as SV
import Foreign (Storable (..), castPtr)

import Pipewire.CContext
import Pipewire.CoreAPI.Core (Core (..))
import Pipewire.CoreAPI.Loop (Loop (..))
import Pipewire.Prelude
import Pipewire.Protocol
import Pipewire.SPA.Utilities.Hooks (SpaHook (..), withSpaHook)
import Pipewire.Utilities.Properties (PwProperties (..))

newtype PwStream = PwStream (Ptr PwStreamStruct) deriving newtype (Storable)
newtype PwStreamEvents = PwStreamEvents (Ptr PwStreamEventsStruct)
newtype PwStreamData = PwStreamData (Ptr (Ptr PwStreamStruct))
newtype PwBuffer = PwBuffer (Ptr PwBufferStruct)

C.context (C.baseCtx <> C.vecCtx <> pwContext)

C.include "<pipewire/stream.h>"
C.include "<pipewire/keys.h>"
C.include "<spa/param/audio/format-utils.h>"

-- data PwStreamEvents = PwStreamEvents (Ptr PwStreamEventsStruct) PwStreamData

type OnProcessHandler = PwStream -> IO ()

withStreamEvents :: (PwStreamEvents -> IO a) -> IO a
withStreamEvents cb =
    allocaBytes (fromIntegral [C.pure| size_t {sizeof (struct pw_stream_events)} |]) (cb . PwStreamEvents)

simpleAudioHandler :: Channels -> (Int -> IO (SV.Vector Float)) -> OnProcessHandler
simpleAudioHandler chans cb pwStream = do
    -- Get the next buffer
    pw_stream_dequeue_bufer pwStream >>= \case
        Just buffer -> do
            -- Check how many samples is needed
            nFrames <- audioFrames chans buffer
            -- Write new samples
            writeAudioFrame buffer =<< cb nFrames
            -- Submit the buffer
            pw_stream_queue_buffer pwStream buffer
        Nothing -> putStrLn "out of buffer"

withAudioHandlers :: PwStream -> OnProcessHandler -> (PwStream -> IO a) -> IO a
withAudioHandlers pwStream@(PwStream s) onProcess cb =
    withSpaHook \(SpaHook spaHook) -> withStreamEvents \(PwStreamEvents streamEvents) -> do
        onProcessP <- $(C.mkFunPtr [t|Ptr () -> IO ()|]) onProcessWrapper
        -- setup pw_stream_events
        [C.block| void{
                struct pw_stream_events* pw_events = $(struct pw_stream_events* streamEvents);
                pw_events->version = PW_VERSION_STREAM_EVENTS;
                pw_events->process = $(void (*onProcessP)(void*));
                pw_stream_add_listener($(struct pw_stream* s),
                    $(struct spa_hook* spaHook),
                    pw_events,
                    NULL);
            }|]
        cb pwStream `finally` do
            freeHaskellFunPtr onProcessP
  where
    onProcessWrapper _data = onProcess pwStream

withAudioStream :: Core -> OnProcessHandler -> (PwStream -> IO a) -> IO a
withAudioStream pwCore onProcess cb = do
    props <-
        PwProperties
            <$> [C.exp| struct pw_properties*{pw_properties_new(
                                            PW_KEY_MEDIA_TYPE, "Audio",
                                            PW_KEY_MEDIA_CATEGORY, "Playback",
                                            PW_KEY_MEDIA_ROLE, "Music",
                                            NULL)}|]
    pwStream <- pw_stream_new pwCore "audio-src" props
    withAudioHandlers pwStream onProcess cb `finally` do
        pw_stream_destroy pwStream

newtype Channels = Channels Int deriving newtype (Show)
newtype Rate = Rate Int deriving newtype (Show)

-- | Connect the stream (using s16 format)
connectAudioStream :: Rate -> Channels -> PwStream -> IO ()
connectAudioStream (Rate (fromIntegral -> rate)) (Channels (fromIntegral -> chans)) (PwStream pwStream) = do
    [C.block| void{
    const struct spa_pod *params[1];
    uint8_t buffer[1024];
    struct spa_pod_builder b = SPA_POD_BUILDER_INIT(buffer, sizeof(buffer));

    params[0] = spa_format_audio_raw_build(&b, SPA_PARAM_EnumFormat,
                        &SPA_AUDIO_INFO_RAW_INIT(
                                .format = SPA_AUDIO_FORMAT_S16,
                                .channels = $(int chans),
                                .rate = $(int rate) ));

    pw_stream_connect($(struct pw_stream* pwStream),
                          PW_DIRECTION_OUTPUT,
                          PW_ID_ANY,
                          PW_STREAM_FLAG_AUTOCONNECT |
                          PW_STREAM_FLAG_MAP_BUFFERS |
                          PW_STREAM_FLAG_RT_PROCESS,
                          params, 1);
  }|]

audioFrames :: Channels -> PwBuffer -> IO Int
audioFrames (Channels (fromIntegral -> chans)) (PwBuffer pwBuffer) =
    fromIntegral
        <$> [C.block| int{
    struct pw_buffer* b = $(struct pw_buffer* pwBuffer);
    struct spa_buffer* buf = b->buffer;
    int stride = sizeof(int16_t) * $(int chans);
    int n_frames = buf->datas[0].maxsize / stride;
    if (b->requested)
      n_frames = SPA_MIN(b->requested, n_frames);

    // This should really be done after writing the audio frame
    buf->datas[0].chunk->offset = 0;
    buf->datas[0].chunk->stride = stride;
    buf->datas[0].chunk->size = n_frames * stride;
    return n_frames;
  }|]

-- | Write the frame (to a s16 formated buffer)
writeAudioFrame :: PwBuffer -> SV.Vector Float -> IO ()
writeAudioFrame (PwBuffer pwBuffer) (cfloatVector -> samples) = do
    [C.block| void {
      struct pw_buffer* b = $(struct pw_buffer* pwBuffer);
      float *src = $vec-ptr:(float *samples);
      int16_t *dst = b->buffer->datas[0].data;
      for (int i = 0; i < $vec-len:samples; i++)
        *dst++ = $vec-ptr:(float *samples)[i] * 32767.0;
    }|]

pw_stream_new_simple :: Loop -> Text -> PwProperties -> PwStreamEvents -> Ptr () -> IO PwStream
pw_stream_new_simple (Loop pwLoop) name (PwProperties props) (PwStreamEvents pwStreamEvents) (castPtr -> dataPtr) =
    withCString name \nameC -> do
        PwStream
            <$> dieOnNull
                "pw_stream_new_simple"
                [C.exp| struct pw_stream*{
               pw_stream_new_simple(
                       $(struct pw_loop* pwLoop),
                       $(const char* nameC),
                       $(struct pw_properties* props),
                       $(struct pw_stream_events* pwStreamEvents),
                       $(void* dataPtr))
               }|]

pw_stream_new :: Core -> Text -> PwProperties -> IO PwStream
pw_stream_new (Core pwCore) name (PwProperties props) =
    withCString name \nameC ->
        PwStream
            <$> dieOnNull
                "pw_stream_new"
                [C.exp| struct pw_stream*{
                   pw_stream_new(
                     $(struct pw_core* pwCore),
                     $(const char* nameC),
                     $(struct pw_properties* props)
                 )}|]

pw_stream_connect :: PwStream -> IO ()
pw_stream_connect (PwStream _pwStream) = pure ()

pw_stream_destroy :: PwStream -> IO ()
pw_stream_destroy (PwStream pwStream) =
    [C.exp| void{pw_stream_destroy($(struct pw_stream* pwStream))} |]

pw_stream_dequeue_bufer :: PwStream -> IO (Maybe PwBuffer)
pw_stream_dequeue_bufer (PwStream pwStream) =
    maybeOnNull PwBuffer
        <$> [C.exp| struct pw_buffer*{pw_stream_dequeue_buffer($(struct pw_stream* pwStream))} |]

pw_stream_queue_buffer :: PwStream -> PwBuffer -> IO ()
pw_stream_queue_buffer (PwStream pwStream) (PwBuffer pwBuffer) =
    [C.exp| void{pw_stream_queue_buffer($(struct pw_stream* pwStream), $(struct pw_buffer* pwBuffer))} |]

pw_stream_trigger_process :: PwStream -> IO ()
pw_stream_trigger_process (PwStream pwStream) =
    [C.exp| void{pw_stream_trigger_process($(struct pw_stream* pwStream))} |]

pw_stream_get_node_id :: PwStream -> IO PwID
pw_stream_get_node_id (PwStream pwStream) =
    PwID . fromIntegral <$> [C.exp| int{pw_stream_get_node_id($(struct pw_stream* pwStream))} |]
