// not upstream: XRSound's audio engine on PipeWire (one float32 stereo 48 kHz playback stream mixed here); replaces the closed-source irrKlang

#include "AudioEngine.h"

#include <algorithm>
#include <cerrno>
#include <chrono>
#include <cmath>
#include <cstring>

#include <pipewire/pipewire.h>
#include <spa/param/audio/format-utils.h>

namespace
{
const uint64_t WindowFrames = 4096;     // stream frames decoded at a time
const int ConnectTimeoutSeconds = 3;    // longest wait for the daemon to take the stream, so Orbiter never hangs on it
}

// PipeWire objects; the callbacks run on the thread loop with its lock held
struct AudioEngine::PipeWire
{
    pw_thread_loop *pLoop = nullptr;
    pw_context *pContext = nullptr;
    pw_core *pCore = nullptr;
    pw_stream *pStream = nullptr;
    spa_hook coreListener{};
    spa_hook streamListener{};
    pw_stream_state state = PW_STREAM_STATE_UNCONNECTED;
    std::string error;
    bool bInitialized = false;

    static const pw_core_events coreEvents;
    static const pw_stream_events streamEvents;

    static void CoreError(void *pData, uint32_t id, int seq, int res, const char *pMessage)
    {
        AudioEngine *pEngine = static_cast<AudioEngine *>(pData);
        if (id != PW_ID_CORE)
            return;     // errors of other objects don't break the connection
        pEngine->m_pw->error = std::string("PipeWire connection: ") + (pMessage ? pMessage : strerror(-res));   // res is a negative errno
        pEngine->m_bFailed = true;
        pw_thread_loop_signal(pEngine->m_pw->pLoop, false);
    }

    static void StreamStateChanged(void *pData, pw_stream_state oldState, pw_stream_state state, const char *pError)
    {
        AudioEngine *pEngine = static_cast<AudioEngine *>(pData);
        pEngine->m_pw->state = state;
        if (state == PW_STREAM_STATE_ERROR)
        {
            pEngine->m_pw->error = std::string("PipeWire stream: ") + (pError ? pError : "error");
            pEngine->m_bFailed = true;
        }
        pw_thread_loop_signal(pEngine->m_pw->pLoop, false);
    }

    static void StreamProcess(void *pData)
    {
        static_cast<AudioEngine *>(pData)->Process();
    }

    static pw_core_events MakeCoreEvents()
    {
        pw_core_events events{};
        events.version = PW_VERSION_CORE_EVENTS;
        events.error = CoreError;
        return events;
    }

    static pw_stream_events MakeStreamEvents()
    {
        pw_stream_events events{};
        events.version = PW_VERSION_STREAM_EVENTS;
        events.state_changed = StreamStateChanged;
        events.process = StreamProcess;
        return events;
    }
};

const pw_core_events AudioEngine::PipeWire::coreEvents = AudioEngine::PipeWire::MakeCoreEvents();
const pw_stream_events AudioEngine::PipeWire::streamEvents = AudioEngine::PipeWire::MakeStreamEvents();

AudioEngine::AudioEngine() :
    m_pw(new PipeWire), m_cacheBytes(0), m_bFailed(false)
{
}

AudioEngine *AudioEngine::Create(const char *pAppName, std::string &error)
{
    AudioEngine *pEngine = new AudioEngine();
    if (!pEngine->Start(pAppName, error))
    {
        delete pEngine;
        return nullptr;
    }
    return pEngine;
}

bool AudioEngine::Start(const char *pAppName, std::string &error)
{
    pw_init(nullptr, nullptr);
    m_pw->bInitialized = true;

    m_pw->pLoop = pw_thread_loop_new("XRSound", nullptr);
    if (!m_pw->pLoop)
    {
        error = std::string("can't create the PipeWire thread loop: ") + strerror(errno);
        return false;
    }
    m_pw->pContext = pw_context_new(pw_thread_loop_get_loop(m_pw->pLoop), nullptr, 0);
    if (!m_pw->pContext)
    {
        error = std::string("can't create a PipeWire context: ") + strerror(errno);
        return false;
    }
    // connect before the loop thread starts: stopping a just-started thread loop can hang in pw_thread_loop_stop
    m_pw->pCore = pw_context_connect(m_pw->pContext, nullptr, 0);   // fails at once when no daemon listens on the socket
    if (!m_pw->pCore)
    {
        error = std::string("no PipeWire daemon: ") + strerror(errno);
        return false;
    }
    pw_core_add_listener(m_pw->pCore, &m_pw->coreListener, &PipeWire::coreEvents, this);

    pw_properties *pProps = pw_properties_new(
        PW_KEY_MEDIA_TYPE, "Audio",
        PW_KEY_MEDIA_CATEGORY, "Playback",
        PW_KEY_MEDIA_ROLE, "Game",
        PW_KEY_APP_NAME, pAppName,
        PW_KEY_NODE_NAME, "XRSound",
        PW_KEY_NODE_LATENCY, "1024/48000",
        nullptr);
    m_pw->pStream = pw_stream_new(m_pw->pCore, "XRSound", pProps);   // takes pProps
    if (!m_pw->pStream)
    {
        error = std::string("can't create a PipeWire stream: ") + strerror(errno);
        return false;
    }
    pw_stream_add_listener(m_pw->pStream, &m_pw->streamListener, &PipeWire::streamEvents, this);

    uint8_t podBuffer[1024];
    spa_pod_builder builder;
    spa_pod_builder_init(&builder, podBuffer, sizeof(podBuffer));
    spa_audio_info_raw info{};
    info.format = SPA_AUDIO_FORMAT_F32;
    info.rate = SampleRate;
    info.channels = Channels;
    info.position[0] = SPA_AUDIO_CHANNEL_FL;
    info.position[1] = SPA_AUDIO_CHANNEL_FR;
    const spa_pod *params[1] = { spa_format_audio_raw_build(&builder, SPA_PARAM_EnumFormat, &info) };

    const pw_stream_flags flags = static_cast<pw_stream_flags>(PW_STREAM_FLAG_AUTOCONNECT | PW_STREAM_FLAG_MAP_BUFFERS);
    if (pw_stream_connect(m_pw->pStream, PW_DIRECTION_OUTPUT, PW_ID_ANY, flags, params, 1) < 0)
    {
        error = "can't connect the PipeWire playback stream";
        return false;
    }

    if (pw_thread_loop_start(m_pw->pLoop) < 0)
    {
        error = "can't start the PipeWire thread loop";
        return false;
    }

    pw_thread_loop_lock(m_pw->pLoop);
    auto fail = [this, &error](const std::string &text)
    {
        error = text;
        pw_thread_loop_unlock(m_pw->pLoop);
        return false;
    };

    // PAUSED means the daemon made our node; STREAMING follows once the session manager links it to a sink
    const auto deadline = std::chrono::steady_clock::now() + std::chrono::seconds(ConnectTimeoutSeconds);
    while (!m_bFailed && (m_pw->state != PW_STREAM_STATE_PAUSED) && (m_pw->state != PW_STREAM_STATE_STREAMING))
    {
        if (std::chrono::steady_clock::now() >= deadline)
            return fail("the PipeWire daemon did not take the playback stream in time");
        pw_thread_loop_timed_wait(m_pw->pLoop, 1);
    }
    if (m_bFailed)
        return fail(m_pw->error);
    pw_thread_loop_unlock(m_pw->pLoop);

    m_driverName = std::string("PipeWire ") + pw_get_library_version() + " (" + std::to_string(SampleRate) + " Hz float32 stereo)";
    return true;
}

AudioEngine::~AudioEngine()
{
    if (m_pw->pLoop)
        pw_thread_loop_stop(m_pw->pLoop);   // the mixer is not called after this
    if (m_pw->pStream)
    {
        if (m_pw->streamListener.link.next)
            spa_hook_remove(&m_pw->streamListener);
        pw_stream_destroy(m_pw->pStream);
    }
    if (m_pw->pCore)
    {
        if (m_pw->coreListener.link.next)
            spa_hook_remove(&m_pw->coreListener);
        pw_core_disconnect(m_pw->pCore);
    }
    if (m_pw->pContext)
        pw_context_destroy(m_pw->pContext);
    if (m_pw->pLoop)
        pw_thread_loop_destroy(m_pw->pLoop);
    if (m_pw->bInitialized)
        pw_deinit();

    for (AudioVoice *pVoice : m_voices)
        delete pVoice;
}

AudioVoice *AudioEngine::Play(const char *pPath, const bool bLoop, const bool bStartPaused, std::string &error)
{
    if (!pPath || !*pPath)
    {
        error = "no file name";
        return nullptr;
    }

    // decoded sounds are cached by path, as irrKlang kept its sound sources; big files keep only their bytes
    const std::string key(pPath);
    std::shared_ptr<AudioBuffer> buffer;
    AudioBytes bytes;
    auto itBuffer = m_bufferCache.find(key);
    if (itBuffer != m_bufferCache.end())
        buffer = itBuffer->second;
    else
    {
        auto itBytes = m_bytesCache.find(key);
        if (itBytes != m_bytesCache.end())
            bytes = itBytes->second;
        else
        {
            std::shared_ptr<std::vector<uint8_t>> file = std::make_shared<std::vector<uint8_t>>();
            if (!AudioDecode::ReadFile(pPath, *file, error))
                return nullptr;
            if (file->size() > StreamThreshold)
            {
                bytes = file;
                m_bytesCache[key] = bytes;
                m_cacheBytes += file->size();
            }
            else
            {
                buffer = AudioDecode::Decode(*file, error);
                if (!buffer)
                    return nullptr;
                m_bufferCache[key] = buffer;
                m_cacheBytes += buffer->samples.size() * sizeof(float);
            }
        }
    }

    AudioVoice *pVoice = new AudioVoice(this);
    if (buffer)
    {
        pVoice->m_channels = buffer->channels;
        pVoice->m_sampleRate = buffer->sampleRate;
        pVoice->m_frames = buffer->frames;
        pVoice->m_buffer = buffer;
    }
    else
    {
        std::unique_ptr<AudioStream> stream = AudioDecode::OpenStream(bytes, error);
        if (!stream)
        {
            delete pVoice;
            return nullptr;
        }
        pVoice->m_channels = stream->channels;
        pVoice->m_sampleRate = stream->sampleRate;
        pVoice->m_frames = stream->frames;
        pVoice->m_window.resize(WindowFrames * stream->channels);
        pVoice->m_stream = std::move(stream);
    }
    pVoice->m_bLoop = bLoop;
    pVoice->m_bPaused = bStartPaused;

    {
        std::lock_guard<std::mutex> lock(m_mutex);
        pVoice->m_bFinished = m_bFailed;    // without an output nothing plays
        m_voices.push_back(pVoice);
    }
    TrimCache();
    return pVoice;
}

void AudioEngine::Update()
{
    std::vector<AudioVoice *> done;
    {
        std::lock_guard<std::mutex> lock(m_mutex);
        const bool bFailed = m_bFailed;
        m_voices.erase(std::remove_if(m_voices.begin(), m_voices.end(), [bFailed, &done](AudioVoice *pVoice)
        {
            if (bFailed)
                pVoice->m_bFinished = true;     // the output is gone, so no sound can play to its end
            const bool bDone = pVoice->m_bReleased && pVoice->m_bFinished;
            if (bDone)
                done.push_back(pVoice);
            return bDone;
        }), m_voices.end());
    }
    for (AudioVoice *pVoice : done)
        delete pVoice;      // outside the lock: freeing a decoder or a sound takes time
    TrimCache();
}

// drops cached sounds no voice plays until the cache fits its budget again (Orbiter thread only)
void AudioEngine::TrimCache()
{
    for (auto it = m_bufferCache.begin(); (it != m_bufferCache.end()) && (m_cacheBytes > CacheBudget); )
    {
        if (it->second.use_count() == 1)
        {
            m_cacheBytes -= it->second->samples.size() * sizeof(float);
            it = m_bufferCache.erase(it);
        }
        else
            ++it;
    }
    for (auto it = m_bytesCache.begin(); (it != m_bytesCache.end()) && (m_cacheBytes > CacheBudget); )
    {
        if (it->second.use_count() == 1)
        {
            m_cacheBytes -= it->second->size();
            it = m_bytesCache.erase(it);
        }
        else
            ++it;
    }
}

void AudioEngine::Process()
{
    pw_buffer *pBuffer = pw_stream_dequeue_buffer(m_pw->pStream);
    if (!pBuffer)
        return;     // no free buffer this time
    spa_data &data = pBuffer->buffer->datas[0];
    float *pOut = static_cast<float *>(data.data);
    if (!pOut)
        return;

    const uint32_t stride = sizeof(float) * Channels;
    uint32_t frames = data.maxsize / stride;
    if ((pBuffer->requested > 0) && (pBuffer->requested < frames))
        frames = static_cast<uint32_t>(pBuffer->requested);
    Mix(pOut, frames);

    data.chunk->offset = 0;
    data.chunk->stride = stride;
    data.chunk->size = frames * stride;
    pw_stream_queue_buffer(m_pw->pStream, pBuffer);
}

void AudioEngine::Mix(float *pOut, const uint32_t frames)
{
    std::fill(pOut, pOut + frames * Channels, 0.0f);
    std::lock_guard<std::mutex> lock(m_mutex);
    for (AudioVoice *pVoice : m_voices)
        pVoice->Mix(pOut, frames, SampleRate);
    for (uint32_t i = 0; i < frames * Channels; i++)
        pOut[i] = std::clamp(pOut[i], -1.0f, 1.0f);    // many loud sounds at once must not wrap around
}

AudioVoice::AudioVoice(AudioEngine *pEngine) :
    m_pEngine(pEngine), m_channels(0), m_sampleRate(0), m_frames(0), m_pos(0), m_volume(1.0f), m_pan(0), m_speed(1.0f),
    m_bLoop(false), m_bPaused(false), m_bFinished(false), m_bReleased(false), m_bGainSet(false), m_gainL(0), m_gainR(0),
    m_winStart(0), m_winFrames(0)
{
}

AudioVoice::~AudioVoice()
{
}

bool AudioVoice::IsFinished() const
{
    std::lock_guard<std::mutex> lock(m_pEngine->m_mutex);
    return m_bFinished;
}

void AudioVoice::Stop()
{
    std::lock_guard<std::mutex> lock(m_pEngine->m_mutex);
    m_bFinished = true;
}

void AudioVoice::Release()
{
    std::lock_guard<std::mutex> lock(m_pEngine->m_mutex);
    m_bReleased = true;
}

void AudioVoice::SetPaused(const bool bPaused)
{
    std::lock_guard<std::mutex> lock(m_pEngine->m_mutex);
    m_bPaused = bPaused;
}

bool AudioVoice::IsPaused() const
{
    std::lock_guard<std::mutex> lock(m_pEngine->m_mutex);
    return m_bPaused;
}

void AudioVoice::SetVolume(const float volume)
{
    std::lock_guard<std::mutex> lock(m_pEngine->m_mutex);
    m_volume = std::clamp(volume, 0.0f, 1.0f);
}

float AudioVoice::GetVolume() const
{
    std::lock_guard<std::mutex> lock(m_pEngine->m_mutex);
    return m_volume;
}

void AudioVoice::SetLooped(const bool bLoop)
{
    std::lock_guard<std::mutex> lock(m_pEngine->m_mutex);
    m_bLoop = bLoop;
}

bool AudioVoice::IsLooped() const
{
    std::lock_guard<std::mutex> lock(m_pEngine->m_mutex);
    return m_bLoop;
}

void AudioVoice::SetPan(const float pan)
{
    std::lock_guard<std::mutex> lock(m_pEngine->m_mutex);
    m_pan = std::clamp(pan, -1.0f, 1.0f);
}

float AudioVoice::GetPan() const
{
    std::lock_guard<std::mutex> lock(m_pEngine->m_mutex);
    return m_pan;
}

bool AudioVoice::SetPlaybackSpeed(const float speed)
{
    if (!(speed > 0) || (speed > 16.0f))
        return false;
    std::lock_guard<std::mutex> lock(m_pEngine->m_mutex);
    m_speed = speed;
    return true;
}

float AudioVoice::GetPlaybackSpeed() const
{
    std::lock_guard<std::mutex> lock(m_pEngine->m_mutex);
    return m_speed;
}

bool AudioVoice::SetPlayPosition(const uint32_t positionMillis)
{
    std::lock_guard<std::mutex> lock(m_pEngine->m_mutex);
    const double frame = static_cast<double>(positionMillis) * m_sampleRate / 1000.0;
    if ((m_frames > 0) && (frame > static_cast<double>(m_frames)))
        return false;
    m_pos = frame;
    return true;
}

int AudioVoice::GetPlayPosition() const
{
    std::lock_guard<std::mutex> lock(m_pEngine->m_mutex);
    return static_cast<int>(m_pos * 1000.0 / m_sampleRate);
}

int AudioVoice::GetLength() const
{
    std::lock_guard<std::mutex> lock(m_pEngine->m_mutex);
    return (m_frames > 0) ? static_cast<int>(static_cast<double>(m_frames) * 1000.0 / m_sampleRate) : -1;
}

void AudioVoice::Mix(float *pOut, const uint32_t frames, const uint32_t outRate)
{
    if (m_bFinished || m_bPaused || (frames == 0))
        return;

    // balance pan: the centre plays both sides at full volume, the far side fades out towards the ends
    const float targetL = m_volume * ((m_pan > 0) ? (1.0f - m_pan) : 1.0f);
    const float targetR = m_volume * ((m_pan < 0) ? (1.0f + m_pan) : 1.0f);
    if (!m_bGainSet)
    {
        m_gainL = targetL;
        m_gainR = targetR;
        m_bGainSet = true;
    }
    const float rampL = (targetL - m_gainL) / frames;
    const float rampR = (targetR - m_gainR) / frames;
    float gainL = m_gainL;
    float gainR = m_gainR;

    // source frames per output frame: sample rate conversion and playback speed in one step, linear interpolation
    const double step = static_cast<double>(m_speed) * m_sampleRate / outRate;
    for (uint32_t i = 0; i < frames; i++)
    {
        if ((m_frames > 0) && (m_pos >= static_cast<double>(m_frames)))
        {
            if (!m_bLoop)
            {
                m_bFinished = true;
                break;
            }
            m_pos = std::fmod(m_pos, static_cast<double>(m_frames));
        }

        uint64_t i0 = static_cast<uint64_t>(m_pos);
        float l0, r0;
        if (!FetchFrame(i0, l0, r0))
        {
            if ((m_frames == 0) && (i0 > 0))
                m_frames = i0;      // a stream of unknown length ended, so now it is known
            if (!m_bLoop || (m_frames == 0))
            {
                m_bFinished = true;
                break;
            }
            m_pos = std::fmod(m_pos, static_cast<double>(m_frames));
            i0 = static_cast<uint64_t>(m_pos);
            if (!FetchFrame(i0, l0, r0))
            {
                m_bFinished = true;
                break;
            }
        }
        float l1 = l0, r1 = r0;     // past the last frame the last one is held
        if (((m_frames == 0) || (i0 + 1 < m_frames)) && !FetchFrame(i0 + 1, l1, r1))
        {
            l1 = l0;
            r1 = r0;
        }

        const float frac = static_cast<float>(m_pos - static_cast<double>(i0));
        gainL += rampL;
        gainR += rampR;
        pOut[2 * i] += (l0 + (l1 - l0) * frac) * gainL;
        pOut[2 * i + 1] += (r0 + (r1 - r0) * frac) * gainR;
        m_pos += step;
    }
    m_gainL = targetL;
    m_gainR = targetR;
}

bool AudioVoice::FetchFrame(const uint64_t index, float &left, float &right)
{
    const float *p;
    if (m_buffer)
    {
        if (index >= m_frames)
            return false;
        p = m_buffer->samples.data() + index * m_channels;
    }
    else
    {
        if (((index < m_winStart) || (index >= m_winStart + m_winFrames)) && !FillWindow(index))
            return false;
        p = m_window.data() + (index - m_winStart) * m_channels;
    }
    if (m_channels <= 2)
    {
        left = p[0];
        right = (m_channels > 1) ? p[1] : p[0];     // mono plays on both sides
        return true;
    }
    // more channels (WAV order FL FR FC LFE BL BR SL SR) fold into two: centre and surrounds at -3 dB, LFE out
    const float k = 0.7071f;
    left = p[0];
    right = p[1];
    switch (m_channels)
    {
    case 3: left += k * p[2]; right += k * p[2]; break;                                         // FL FR FC
    case 4: left += k * p[2]; right += k * p[3]; break;                                         // FL FR BL BR
    case 5: left += k * (p[2] + p[3]); right += k * (p[2] + p[4]); break;                       // FL FR FC BL BR
    case 6: left += k * (p[2] + p[4]); right += k * (p[2] + p[5]); break;                       // FL FR FC LFE BL BR
    case 7: left += k * (p[2] + p[5]) + 0.5f * p[4]; right += k * (p[2] + p[6]) + 0.5f * p[4]; break; // FL FR FC LFE BC SL SR
    default: left += k * (p[2] + p[4] + p[6]); right += k * (p[2] + p[5] + p[7]); break;         // FL FR FC LFE BL BR SL SR
    }
    return true;
}

// decodes the stream window that holds index; false at the end of the stream
bool AudioVoice::FillWindow(const uint64_t index)
{
    if ((m_winFrames == 0) || (index < m_winStart) || (index > m_winStart + m_winFrames + WindowFrames))
    {
        // a jump: first play, loop restart, new play position, or a speed that skips a whole window
        m_winFrames = 0;
        if (!m_stream->Seek(index))
            return false;
        m_winStart = index;
        m_winFrames = m_stream->Read(m_window.data(), WindowFrames);
        return (m_winFrames > 0);
    }
    while (index >= m_winStart + m_winFrames)
    {
        // read on, keeping the last frame so an interpolation pair never straddles two windows
        memmove(m_window.data(), m_window.data() + (m_winFrames - 1) * m_channels, m_channels * sizeof(float));
        m_winStart += m_winFrames - 1;
        const uint64_t n = m_stream->Read(m_window.data() + m_channels, WindowFrames - 1);
        m_winFrames = 1 + n;
        if (n == 0)
            return false;
    }
    return true;
}
