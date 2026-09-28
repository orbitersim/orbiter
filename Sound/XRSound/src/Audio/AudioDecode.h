// not upstream: sound file decoding for XRSound's audio engine (WAV parsed here; OGG, MP3, FLAC via stb_vorbis, dr_mp3, dr_flac; MOD, S3M, XM, IT via libxmp-lite)

#pragma once

#include <cstdint>
#include <memory>
#include <string>
#include <vector>

// a file's bytes, shared by the engine's cache and the streams that decode from them
typedef std::shared_ptr<const std::vector<uint8_t>> AudioBytes;

// a whole sound decoded to interleaved float samples
struct AudioBuffer
{
    std::vector<float> samples;   // frames * channels
    uint32_t channels = 0;
    uint32_t sampleRate = 0;
    uint64_t frames = 0;
};

// decodes a sound piece by piece from its file bytes in memory (long sounds such as music)
class AudioStream
{
public:
    virtual ~AudioStream() { }
    virtual uint64_t Read(float *pOut, const uint64_t frameCount) = 0;   // interleaved; returns frames read, 0 at the end
    virtual bool Seek(const uint64_t frame) = 0;

    uint32_t channels = 0;
    uint32_t sampleRate = 0;
    uint64_t frames = 0;    // 0 if the length is unknown (MP3)
};

namespace AudioDecode
{
    // reads a whole file; false and an error text if it can't be read
    bool ReadFile(const char *pPath, std::vector<uint8_t> &bytes, std::string &error);

    // decodes all of a file's bytes; nullptr and an error text if the format is unknown or the data is broken
    std::shared_ptr<AudioBuffer> Decode(const std::vector<uint8_t> &bytes, std::string &error);

    // opens a decoder that reads the bytes on demand; nullptr and an error text on failure
    std::unique_ptr<AudioStream> OpenStream(const AudioBytes &bytes, std::string &error);
}
