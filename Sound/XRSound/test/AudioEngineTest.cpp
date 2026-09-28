// not upstream: XRSound mixer test; decodes asset sounds, checks streaming against whole decoding, plays 0.5 s through PipeWire

#include "Audio/AudioEngine.h"

#include <algorithm>
#include <chrono>
#include <cmath>
#include <cstdio>
#include <string>
#include <thread>

static int s_failures = 0;

static void Check(const bool bOk, const char *pWhat)
{
    printf("%s: %s\n", bOk ? "ok  " : "FAIL", pWhat);
    if (!bOk)
        s_failures++;
}

int main(int argc, char **argv)
{
    const std::string dir = (argc > 1) ? argv[1] : XRSOUND_TEST_SOUNDS;
    const std::string wav = dir + "/Welcome Aboard All Systems Nominal.wav";
    const std::string ogg = dir + "/Music/Solar Serenity.ogg";
    std::string error;

    // whole decoding of a WAV
    std::vector<uint8_t> bytes;
    Check(AudioDecode::ReadFile(wav.c_str(), bytes, error), ("read " + wav + " " + error).c_str());
    std::shared_ptr<AudioBuffer> buffer = AudioDecode::Decode(bytes, error);
    Check(buffer != nullptr, ("decode WAV " + error).c_str());
    if (buffer)
    {
        printf("      %u ch, %u Hz, %llu frames (%.2f s)\n", buffer->channels, buffer->sampleRate,
            static_cast<unsigned long long>(buffer->frames), static_cast<double>(buffer->frames) / buffer->sampleRate);
        Check((buffer->frames > 0) && (buffer->samples.size() == buffer->frames * buffer->channels), "WAV sample count");
        float peak = 0;
        for (float s : buffer->samples)
            peak = std::max(peak, std::fabs(s));
        Check((peak > 0.01f) && (peak <= 1.0f), "WAV samples in -1..1 and not silent");

        // the stream decoder must give the same samples
        AudioBytes shared = std::make_shared<std::vector<uint8_t>>(bytes);
        std::unique_ptr<AudioStream> stream = AudioDecode::OpenStream(shared, error);
        Check(stream != nullptr, ("open WAV stream " + error).c_str());
        if (stream)
        {
            std::vector<float> all(buffer->samples.size() + 64);
            uint64_t got = 0, n;
            while ((n = stream->Read(all.data() + got * stream->channels, 1000)) > 0)
                got += n;
            Check((got == buffer->frames) && std::equal(buffer->samples.begin(), buffer->samples.end(), all.begin()), "WAV stream equals whole decode");
        }
    }

    // OGG Vorbis stream (the music is streamed): open, read one second, seek
    std::vector<uint8_t> oggBytes;
    if (AudioDecode::ReadFile(ogg.c_str(), oggBytes, error))
    {
        AudioBytes shared = std::make_shared<std::vector<uint8_t>>(std::move(oggBytes));
        std::unique_ptr<AudioStream> stream = AudioDecode::OpenStream(shared, error);
        Check(stream != nullptr, ("open OGG stream " + error).c_str());
        if (stream)
        {
            printf("      %u ch, %u Hz, %llu frames (%.1f s)\n", stream->channels, stream->sampleRate,
                static_cast<unsigned long long>(stream->frames), static_cast<double>(stream->frames) / stream->sampleRate);
            std::vector<float> second(stream->sampleRate * stream->channels);
            Check(stream->Read(second.data(), stream->sampleRate) == stream->sampleRate, "OGG read 1 s");
            Check(stream->Seek(stream->frames / 2) && (stream->Read(second.data(), 100) == 100), "OGG seek to the middle");
        }
    }
    else
        printf("skip: %s: %s\n", ogg.c_str(), error.c_str());

    // playback; no PipeWire daemon is not a failure, the engine must just say so quickly
    const auto t0 = std::chrono::steady_clock::now();
    AudioEngine *pEngine = AudioEngine::Create("XRSound test", error);
    const double createSeconds = std::chrono::duration<double>(std::chrono::steady_clock::now() - t0).count();
    Check(createSeconds < 4.0, "engine create returns within 4 s");
    if (!pEngine)
    {
        printf("skip: sound disabled: %s (%.3f s)\n", error.c_str(), createSeconds);
        return s_failures ? 1 : 0;
    }
    printf("      driver %s (%.3f s)\n", pEngine->GetDriverName(), createSeconds);

    AudioVoice *pVoice = pEngine->Play(wav.c_str(), false, true, error);
    Check(pVoice != nullptr, ("play " + error).c_str());
    if (pVoice)
    {
        pVoice->SetVolume(0.3f);
        pVoice->SetPaused(false);
        for (int i = 0; i < 10; i++)
        {
            std::this_thread::sleep_for(std::chrono::milliseconds(50));
            pEngine->Update();
        }
        const int pos = pVoice->GetPlayPosition();
        printf("      position after 0.5 s: %d ms of %d ms\n", pos, pVoice->GetLength());
        if (pos <= 0)
            printf("warn: no playback progress (no sink linked to the stream?)\n");
        else
            Check((pos > 300) && (pos < 800), "position advanced about 0.5 s");
        pVoice->Stop();
        Check(pVoice->IsFinished(), "stopped voice is finished");
        pVoice->Release();
        pEngine->Update();
    }
    delete pEngine;
    printf("%s\n", s_failures ? "FAILED" : "PASSED");
    return s_failures ? 1 : 0;
}
