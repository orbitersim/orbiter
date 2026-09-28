// not upstream: sound file decoding for XRSound's audio engine (WAV parsed here; OGG, MP3, FLAC via stb_vorbis, dr_mp3, dr_flac; MOD, S3M, XM, IT via libxmp-lite)

#include "AudioDecode.h"

#include <algorithm>
#include <cerrno>
#include <climits>
#include <cstdio>
#include <cstring>

// the decoders are public-domain single-file libraries fetched by CMake; they decode from memory only
#define DR_MP3_IMPLEMENTATION
#define DR_MP3_NO_STDIO
#include "dr_mp3.h"
#define DR_FLAC_IMPLEMENTATION
#define DR_FLAC_NO_STDIO
#include "dr_flac.h"
#define STB_VORBIS_NO_PUSHDATA_API
#define STB_VORBIS_NO_STDIO
#include "stb_vorbis.c"
#include <xmp.h>

namespace
{
enum class Format { Unknown, Wav, Ogg, Flac, Mp3, Module };

uint16_t Le16(const uint8_t *p)
{
    return static_cast<uint16_t>(p[0] | (p[1] << 8));
}

uint32_t Le32(const uint8_t *p)
{
    return static_cast<uint32_t>(p[0]) | (static_cast<uint32_t>(p[1]) << 8) | (static_cast<uint32_t>(p[2]) << 16) | (static_cast<uint32_t>(p[3]) << 24);
}

// the file's magic bytes decide the format, not its extension
Format Detect(const std::vector<uint8_t> &b)
{
    const uint8_t *p = b.data();
    const size_t n = b.size();
    if ((n >= 12) && !memcmp(p, "RIFF", 4) && !memcmp(p + 8, "WAVE", 4))
        return Format::Wav;
    if ((n >= 4) && !memcmp(p, "OggS", 4))
        return Format::Ogg;
    if ((n >= 4) && !memcmp(p, "fLaC", 4))
        return Format::Flac;
    if ((n >= 3) && !memcmp(p, "ID3", 3))
        return Format::Mp3;
    if ((n >= 2) && (p[0] == 0xFF) && ((p[1] & 0xE0) == 0xE0))   // MPEG audio frame sync
        return Format::Mp3;
    if ((n > 0) && (n <= LONG_MAX) && (xmp_test_module_from_memory(p, static_cast<long>(n), nullptr) == 0))   // tracker formats have no magic at the start
        return Format::Module;
    return Format::Unknown;
}

// what the WAV "fmt " and "data" chunks say
struct WavInfo
{
    uint16_t format = 0;        // 1 = integer PCM, 3 = IEEE float
    uint16_t channels = 0;
    uint32_t sampleRate = 0;
    uint16_t blockAlign = 0;    // bytes per frame
    uint16_t bits = 0;
    const uint8_t *pData = nullptr;
    uint64_t frames = 0;
};

bool ParseWav(const std::vector<uint8_t> &b, WavInfo &w, std::string &error)
{
    bool bHaveFmt = false;
    uint64_t dataBytes = 0;
    size_t pos = 12;    // after "RIFF" <size> "WAVE"
    while (pos + 8 <= b.size())
    {
        const uint8_t *pChunk = b.data() + pos;
        const uint32_t size = Le32(pChunk + 4);
        const size_t avail = b.size() - (pos + 8);
        if (!memcmp(pChunk, "fmt ", 4) && (size >= 16) && (avail >= 16))
        {
            w.format = Le16(pChunk + 8);
            w.channels = Le16(pChunk + 10);
            w.sampleRate = Le32(pChunk + 12);
            w.blockAlign = Le16(pChunk + 20);
            w.bits = Le16(pChunk + 22);
            if ((w.format == 0xFFFE) && (size >= 40) && (avail >= 40))   // WAVE_FORMAT_EXTENSIBLE: the sub-format GUID starts with the format code
                w.format = Le16(pChunk + 8 + 24);
            bHaveFmt = true;
        }
        else if (!memcmp(pChunk, "data", 4))
        {
            w.pData = pChunk + 8;
            dataBytes = std::min<uint64_t>(size, avail);   // a truncated file plays what it has
            if (bHaveFmt)
                break;
        }
        pos += 8 + static_cast<size_t>(size) + (size & 1);   // chunks are padded to an even size
    }

    if (!bHaveFmt || !w.pData)
    {
        error = "WAV file without fmt or data chunk";
        return false;
    }
    if ((w.channels == 0) || (w.sampleRate == 0) || (w.blockAlign == 0) || (w.blockAlign % w.channels))
    {
        error = "WAV file with a broken fmt chunk";
        return false;
    }
    const uint16_t sampleBytes = w.blockAlign / w.channels;   // container size of one sample
    const bool bSupported = ((w.format == 1) && (sampleBytes <= 4)) || ((w.format == 3) && ((sampleBytes == 4) || (sampleBytes == 8)));
    if (!bSupported)
    {
        error = "unsupported WAV sample format " + std::to_string(w.format) + " with " + std::to_string(w.bits) + " bits";
        return false;
    }
    w.frames = dataBytes / w.blockAlign;
    return true;
}

// one WAV sample to -1..1 (x86-64 and AArch64 are little-endian, as WAV is)
inline float WavSample(const uint8_t *p, const uint16_t format, const uint16_t sampleBytes)
{
    if (format == 3)
    {
        if (sampleBytes == 4)
        {
            float f;
            memcpy(&f, p, 4);
            return f;
        }
        double d;
        memcpy(&d, p, 8);
        return static_cast<float>(d);
    }
    switch (sampleBytes)
    {
    case 1:
        return (static_cast<int>(p[0]) - 128) / 128.0f;    // 8-bit WAV is unsigned
    case 2:
        return static_cast<int16_t>(Le16(p)) / 32768.0f;
    case 3:
        return static_cast<int32_t>((static_cast<uint32_t>(p[0]) << 8) | (static_cast<uint32_t>(p[1]) << 16) | (static_cast<uint32_t>(p[2]) << 24)) / 2147483648.0f;
    default:
        return static_cast<int32_t>(Le32(p)) / 2147483648.0f;
    }
}

void ConvertWav(const WavInfo &w, const uint64_t firstFrame, const uint64_t frameCount, float *pOut)
{
    const uint16_t sampleBytes = w.blockAlign / w.channels;
    const uint8_t *p = w.pData + firstFrame * w.blockAlign;
    const uint64_t samples = frameCount * w.channels;
    for (uint64_t i = 0; i < samples; i++, p += sampleBytes)
        pOut[i] = WavSample(p, w.format, sampleBytes);
}

class WavStream : public AudioStream
{
public:
    WavStream(const AudioBytes &bytes, const WavInfo &info) : m_bytes(bytes), m_info(info), m_frame(0)
    {
        channels = info.channels;
        sampleRate = info.sampleRate;
        frames = info.frames;
    }

    uint64_t Read(float *pOut, const uint64_t frameCount) override
    {
        const uint64_t n = std::min(frameCount, frames - m_frame);
        ConvertWav(m_info, m_frame, n, pOut);
        m_frame += n;
        return n;
    }

    bool Seek(const uint64_t frame) override
    {
        if (frame > frames)
            return false;
        m_frame = frame;
        return true;
    }

private:
    AudioBytes m_bytes;     // keeps m_info.pData valid
    WavInfo m_info;
    uint64_t m_frame;
};

class OggStream : public AudioStream
{
public:
    OggStream(const AudioBytes &bytes, stb_vorbis *pVorbis) : m_bytes(bytes), m_pVorbis(pVorbis)
    {
        const stb_vorbis_info info = stb_vorbis_get_info(pVorbis);
        channels = static_cast<uint32_t>(info.channels);
        sampleRate = info.sample_rate;
        frames = stb_vorbis_stream_length_in_samples(pVorbis);
    }

    ~OggStream() override
    {
        stb_vorbis_close(m_pVorbis);
    }

    uint64_t Read(float *pOut, const uint64_t frameCount) override
    {
        uint64_t done = 0;
        while (done < frameCount)
        {
            const int want = static_cast<int>(std::min<uint64_t>(frameCount - done, 65536) * channels);
            const int n = stb_vorbis_get_samples_float_interleaved(m_pVorbis, static_cast<int>(channels), pOut + done * channels, want);
            if (n <= 0)
                break;
            ToWavOrder(pOut + done * channels, static_cast<uint64_t>(n));
            done += static_cast<uint64_t>(n);
        }
        return done;
    }

    bool Seek(const uint64_t frame) override
    {
        return stb_vorbis_seek(m_pVorbis, static_cast<unsigned int>(frame)) != 0;
    }

private:
    // Vorbis orders 3 to 8 channels its own way; the mixer takes WAV's (FL FR FC LFE BL BR SL SR)
    void ToWavOrder(float *p, const uint64_t n) const
    {
        static const int order[9][8] = {
            {}, {}, {},
            { 0, 2, 1 },                        // L C R
            { 0, 1, 2, 3 },                     // FL FR RL RR
            { 0, 2, 1, 3, 4 },                  // FL C FR RL RR
            { 0, 2, 1, 5, 3, 4 },               // FL C FR RL RR LFE
            { 0, 2, 1, 6, 5, 3, 4 },            // FL C FR SL SR RC LFE
            { 0, 2, 1, 7, 5, 6, 3, 4 } };       // FL C FR SL SR RL RR LFE
        if ((channels < 3) || (channels > 8))
            return;
        float f[8];
        for (uint64_t i = 0; i < n; i++, p += channels)
        {
            memcpy(f, p, channels * sizeof(float));
            for (uint32_t c = 0; c < channels; c++)
                p[c] = f[order[channels][c]];
        }
    }

    AudioBytes m_bytes;     // stb_vorbis reads from these bytes
    stb_vorbis *m_pVorbis;
};

class ModStream : public AudioStream
{
public:
    ModStream(const AudioBytes &bytes) : m_bytes(bytes), m_ctx(xmp_create_context()), m_bLoaded(false), m_bEnd(false) { }

    ~ModStream() override
    {
        if (m_bLoaded)
        {
            xmp_end_player(m_ctx);
            xmp_release_module(m_ctx);
        }
        xmp_free_context(m_ctx);
    }

    // renders the song once through at the engine's rate, 16-bit stereo
    bool Open(std::string &error)
    {
        if (xmp_load_module_from_memory(m_ctx, m_bytes->data(), static_cast<long>(m_bytes->size())) != 0)
        {
            error = "broken tracker module";
            return false;
        }
        m_bLoaded = true;
        if (xmp_start_player(m_ctx, Rate, 0) != 0)
        {
            error = "tracker module player failed to start";
            return false;
        }
        xmp_frame_info fi;
        xmp_get_frame_info(m_ctx, &fi);
        channels = 2;
        sampleRate = Rate;
        frames = static_cast<uint64_t>(fi.total_time) * Rate / 1000;
        return true;
    }

    uint64_t Read(float *pOut, const uint64_t frameCount) override
    {
        uint64_t done = 0;
        int16_t pcm[2 * 1024];
        while ((done < frameCount) && !m_bEnd)
        {
            const uint64_t n = std::min<uint64_t>(frameCount - done, 1024);
            if (xmp_play_buffer(m_ctx, pcm, static_cast<int>(n * 4), 1) != 0)   // loop 1: once through, then the end
            {
                m_bEnd = true;
                break;
            }
            for (uint64_t i = 0; i < 2 * n; i++)
                pOut[2 * done + i] = pcm[i] / 32768.0f;
            done += n;
        }
        return done;
    }

    bool Seek(const uint64_t frame) override
    {
        m_bEnd = false;
        if (frame == 0)
        {
            xmp_restart_module(m_ctx);
            xmp_play_buffer(m_ctx, nullptr, 0, 0);   // resets the loop count
            return true;
        }
        return xmp_seek_time(m_ctx, static_cast<int>(frame * 1000 / Rate)) >= 0;
    }

private:
    static const int Rate = 48000;
    AudioBytes m_bytes;     // libxmp copies what it needs while loading
    xmp_context m_ctx;
    bool m_bLoaded, m_bEnd;
};

class Mp3Stream : public AudioStream
{
public:
    Mp3Stream(const AudioBytes &bytes) : m_bytes(bytes), m_bOpen(false) { }

    ~Mp3Stream() override
    {
        if (m_bOpen)
            drmp3_uninit(&m_mp3);
    }

    // the length stays unknown: counting MP3 frames means parsing the whole file
    bool Open()
    {
        m_bOpen = drmp3_init_memory(&m_mp3, m_bytes->data(), m_bytes->size(), nullptr);
        if (m_bOpen)
        {
            channels = m_mp3.channels;
            sampleRate = m_mp3.sampleRate;
        }
        return m_bOpen;
    }

    uint64_t Read(float *pOut, const uint64_t frameCount) override
    {
        return drmp3_read_pcm_frames_f32(&m_mp3, frameCount, pOut);
    }

    bool Seek(const uint64_t frame) override
    {
        return drmp3_seek_to_pcm_frame(&m_mp3, frame) != 0;
    }

private:
    AudioBytes m_bytes;     // dr_mp3 reads from these bytes
    drmp3 m_mp3;
    bool m_bOpen;
};

class FlacStream : public AudioStream
{
public:
    FlacStream(const AudioBytes &bytes, drflac *pFlac) : m_bytes(bytes), m_pFlac(pFlac)
    {
        channels = pFlac->channels;
        sampleRate = pFlac->sampleRate;
        frames = pFlac->totalPCMFrameCount;
    }

    ~FlacStream() override
    {
        drflac_close(m_pFlac);
    }

    uint64_t Read(float *pOut, const uint64_t frameCount) override
    {
        return drflac_read_pcm_frames_f32(m_pFlac, frameCount, pOut);
    }

    bool Seek(const uint64_t frame) override
    {
        return drflac_seek_to_pcm_frame(m_pFlac, frame) != 0;
    }

private:
    AudioBytes m_bytes;     // dr_flac reads from these bytes
    drflac *m_pFlac;
};
}

bool AudioDecode::ReadFile(const char *pPath, std::vector<uint8_t> &bytes, std::string &error)
{
    FILE *pFile = fopen(pPath, "rb");
    if (!pFile)
    {
        error = std::string("can't open the file: ") + strerror(errno);
        return false;
    }
    bool bOk = (fseek(pFile, 0, SEEK_END) == 0);
    const long size = (bOk ? ftell(pFile) : -1);
    bOk = bOk && (size >= 0) && (fseek(pFile, 0, SEEK_SET) == 0);
    if (bOk)
    {
        bytes.resize(static_cast<size_t>(size));
        bOk = (fread(bytes.data(), 1, bytes.size(), pFile) == bytes.size());
    }
    fclose(pFile);
    if (!bOk)
        error = "can't read the file";
    return bOk;
}

std::unique_ptr<AudioStream> AudioDecode::OpenStream(const AudioBytes &bytes, std::string &error)
{
    std::unique_ptr<AudioStream> stream;
    switch (Detect(*bytes))
    {
    case Format::Wav:
    {
        WavInfo info;
        if (ParseWav(*bytes, info, error))
            stream.reset(new WavStream(bytes, info));
        break;
    }

    case Format::Ogg:
    {
        if (bytes->size() > INT_MAX)
        {
            error = "OGG file too large";
            break;
        }
        int err = 0;
        stb_vorbis *pVorbis = stb_vorbis_open_memory(bytes->data(), static_cast<int>(bytes->size()), &err, nullptr);
        if (pVorbis)
            stream.reset(new OggStream(bytes, pVorbis));
        else
            error = "broken OGG Vorbis data (stb_vorbis error " + std::to_string(err) + ")";
        break;
    }

    case Format::Mp3:
    {
        std::unique_ptr<Mp3Stream> mp3(new Mp3Stream(bytes));
        if (mp3->Open())
            stream = std::move(mp3);
        else
            error = "broken MP3 data";
        break;
    }

    case Format::Flac:
    {
        drflac *pFlac = drflac_open_memory(bytes->data(), bytes->size(), nullptr);
        if (pFlac)
            stream.reset(new FlacStream(bytes, pFlac));
        else
            error = "broken FLAC data";
        break;
    }

    case Format::Module:
    {
        std::unique_ptr<ModStream> mod(new ModStream(bytes));
        if (mod->Open(error))
            stream = std::move(mod);
        break;
    }

    default:
        error = "unknown sound file format (WAV, OGG Vorbis, MP3, FLAC and MOD, S3M, XM, IT modules are supported)";
        break;
    }

    if (stream && ((stream->channels == 0) || (stream->sampleRate == 0)))
    {
        error = "sound file without channels or sample rate";
        stream.reset();
    }
    return stream;
}

std::shared_ptr<AudioBuffer> AudioDecode::Decode(const std::vector<uint8_t> &bytes, std::string &error)
{
    const AudioBytes view(&bytes, [](const std::vector<uint8_t> *) { });   // not owned: the stream is gone before this returns
    std::unique_ptr<AudioStream> stream = OpenStream(view, error);
    if (!stream)
        return nullptr;

    std::shared_ptr<AudioBuffer> buffer = std::make_shared<AudioBuffer>();
    buffer->channels = stream->channels;
    buffer->sampleRate = stream->sampleRate;
    if (stream->frames > 0)
        buffer->samples.reserve(stream->frames * stream->channels);

    const uint64_t chunk = 16384;
    for (;;)
    {
        const size_t used = buffer->samples.size();
        buffer->samples.resize(used + chunk * buffer->channels);
        const uint64_t n = stream->Read(buffer->samples.data() + used, chunk);
        buffer->samples.resize(used + n * buffer->channels);
        if (n == 0)
            break;
    }
    buffer->samples.shrink_to_fit();
    buffer->frames = buffer->samples.size() / buffer->channels;
    if (buffer->frames == 0)
    {
        error = "sound file without samples";
        return nullptr;
    }
    return buffer;
}
