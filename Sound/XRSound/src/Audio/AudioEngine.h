// not upstream: XRSound's audio engine on PipeWire (one float32 stereo 48 kHz playback stream mixed here); replaces the closed-source irrKlang

#pragma once

#include "AudioDecode.h"

#include <atomic>
#include <cstdint>
#include <memory>
#include <mutex>
#include <string>
#include <unordered_map>
#include <vector>

class AudioEngine;

// one playing sound: the caller holds it until Release, the engine frees it once finished; methods lock the engine
class AudioVoice
{
public:
    bool IsFinished() const;            // played to the end, stopped, or the sound output failed
    void Stop();
    void Release();                     // the caller is done with the handle

    void SetPaused(const bool bPaused);
    bool IsPaused() const;
    void SetVolume(const float volume); // 0 (muted) .. 1 (full)
    float GetVolume() const;
    void SetLooped(const bool bLoop);
    bool IsLooped() const;
    void SetPan(const float pan);       // -1 full left .. 0 centre .. 1 full right
    float GetPan() const;
    bool SetPlaybackSpeed(const float speed);   // 1 = normal; resampled, so the pitch follows the speed
    float GetPlaybackSpeed() const;
    bool SetPlayPosition(const uint32_t positionMillis);
    int GetPlayPosition() const;        // milliseconds
    int GetLength() const;              // milliseconds, -1 if not known yet

private:
    friend class AudioEngine;
    AudioVoice(AudioEngine *pEngine);
    ~AudioVoice();

    void Mix(float *pOut, const uint32_t frames, const uint32_t outRate);   // mixer thread, engine lock held
    bool FetchFrame(const uint64_t index, float &left, float &right);
    bool FillWindow(const uint64_t index);

    AudioEngine *m_pEngine;
    std::shared_ptr<AudioBuffer> m_buffer;  // the whole sound, or
    std::unique_ptr<AudioStream> m_stream;  // a decoder read while playing
    uint32_t m_channels;
    uint32_t m_sampleRate;
    uint64_t m_frames;          // 0 until a stream of unknown length reaches its end
    double m_pos;               // play position in source frames
    float m_volume;
    float m_pan;
    float m_speed;
    bool m_bLoop;
    bool m_bPaused;
    bool m_bFinished;
    bool m_bReleased;
    bool m_bGainSet;
    float m_gainL;              // gains of the last mixed frame; changes ramp over one period to avoid clicks
    float m_gainR;
    std::vector<float> m_window;    // decoded stream frames [m_winStart, m_winStart + m_winFrames)
    uint64_t m_winStart;
    uint64_t m_winFrames;
};

class AudioEngine
{
public:
    // connects a playback stream to the PipeWire daemon; nullptr and an error text if no daemon answers (sound is then disabled)
    static AudioEngine *Create(const char *pAppName, std::string &error);
    ~AudioEngine();

    const char *GetDriverName() const { return m_driverName.c_str(); }

    // starts a sound from a file (WAV, OGG, MP3, FLAC); nullptr and an error text if it can't be read or decoded
    AudioVoice *Play(const char *pPath, const bool bLoop, const bool bStartPaused, std::string &error);

    // frees released voices that have finished and trims the sound cache; call a few times per second
    void Update();

    static constexpr uint32_t SampleRate = 48000;
    static constexpr uint32_t Channels = 2;
    static constexpr size_t StreamThreshold = 4 * 1024 * 1024;  // larger files are decoded while playing, not up front
    static constexpr size_t CacheBudget = 256 * 1024 * 1024;    // bytes of cached sounds kept when no voice uses them

private:
    friend class AudioVoice;
    struct PipeWire;    // PipeWire objects and callbacks, kept out of this header

    AudioEngine();
    bool Start(const char *pAppName, std::string &error);
    void Process();     // PipeWire asks for the next period
    void Mix(float *pOut, const uint32_t frames);
    void TrimCache();

    std::unique_ptr<PipeWire> m_pw;
    mutable std::mutex m_mutex;         // guards the voices between the Orbiter thread and the mixer thread
    std::vector<AudioVoice *> m_voices;
    std::unordered_map<std::string, std::shared_ptr<AudioBuffer>> m_bufferCache;  // key = file path
    std::unordered_map<std::string, AudioBytes> m_bytesCache;                     // streamed files
    size_t m_cacheBytes;
    std::atomic<bool> m_bFailed;        // the stream or the daemon connection broke
    std::string m_driverName;
};
