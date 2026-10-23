// ==============================================================
// miniaudio implementation of the ISound API
// 
// Copyright (c) 2025 Gondos
// Licensed under the MIT License
// ==============================================================
#include "stb_vorbis.c"
#define MINIAUDIO_IMPLEMENTATION
#include "miniaudio.h"
#include "ISound.h"

#include "OrbiterAPI.h"

#ifndef NDEBUG
#include <imgui.h>
static std::list<ISound *> g_soundList;

class ISoundDialog: public ImGuiDialog
{
public:
    ISoundDialog():ImGuiDialog("Audio debug") {}
    void OnDraw() override {
        if (ImGui::BeginTable("table", 3, ImGuiTableFlags_SizingFixedFit | ImGuiTableFlags_Resizable)) {
            ImGui::TableSetupColumn("State", ImGuiTableColumnFlags_WidthFixed, 70.0f);
            ImGui::TableSetupColumn("Type", ImGuiTableColumnFlags_WidthFixed, 70.0f);
            ImGui::TableSetupColumn("Filename", ImGuiTableColumnFlags_WidthStretch);
            for (const auto &sound: g_soundList) {
                ImGui::TableNextRow();
                ImGui::TableNextColumn();
                switch (sound->m_state) {
                    case ISound::State::PLAYING: ImGui::TextUnformatted("PLAYING"); break;
                    case ISound::State::PAUSED:  ImGui::TextUnformatted("PAUSED"); break;
                    case ISound::State::STOPPED: ImGui::TextUnformatted("STOPPED"); break;
                }

                ImGui::TableNextColumn();
                if (ma_sound_is_looping(&sound->m_sound)) {
                    ImGui::TextUnformatted("Looping");
                } else {
                    ImGui::TextUnformatted("One shot");
                }

                ImGui::TableNextColumn();
                ImGui::TextUnformatted(sound->m_filename.c_str());
            }
            ImGui::EndTable();
        }
    }
};


static void OpenSoundDlgClbk(void *context)
{
    ISoundDialog *dbg = (ISoundDialog *)context;
    oapiOpenDialog(dbg);
}
#endif

ISoundEngine::ISoundEngine()
{
#ifndef NDEBUG
    m_dbgWin = new ISoundDialog();
    m_dwdbgWin = oapiRegisterCustomCmd((char*)"Audio debug", (char*)"This dialog allows to visualize active sound instances", OpenSoundDlgClbk, m_dbgWin);
#endif

    if (ma_engine_init(nullptr, &m_engine) != MA_SUCCESS) {
        oapiWriteLog("XRSound: Error initialising miniaudio engine");
        return;
    }
    m_initialized = true;
}

ISoundEngine::~ISoundEngine()
{
#ifndef NDEBUG
    if (m_dwdbgWin) oapiUnregisterCustomCmd(m_dwdbgWin);
    delete m_dbgWin;
#endif

    if (!m_initialized) return;

    for (auto &snd: releasedSounds) {
        delete snd;
    }
    ma_engine_uninit(&m_engine);

}

ISound *ISoundEngine::play2D(const char *soundFileName, const bool playLooped, const bool startPaused, const bool track)
{
    if (!m_initialized) return nullptr;

    CheckReleased();
    ISound *snd = nullptr;
    try {
        snd = new ISound(&m_engine, soundFileName, playLooped, startPaused);
    }
    catch (...) {
        std::string message = std::string("XRSound: Error loading sound file: ") + soundFileName;
        oapiWriteLog(message.data());
        return nullptr;
    }

    if (startPaused || track)
        return snd;

    Release(snd);
    return nullptr;
}

void ISoundEngine::Release(ISound *snd)
{
    if (!m_initialized) return;
    if (!snd) return;

    CheckReleased();
    if (snd->isFinished())
        delete snd;
    else
        releasedSounds.push_back(snd);
}

void ISoundEngine::CheckReleased()
{
    for (auto it = releasedSounds.begin(); it != releasedSounds.end();) {
        if ((*it)->isFinished()) {
            delete *it;
            it = releasedSounds.erase(it);
        } else {
            ++it;
        }
    }
}


ISound::ISound(ma_engine* engine,
               const char* filename,
               const bool looped,
               const bool paused)
{
    if (ma_result r = ma_sound_init_from_file(
            engine,
            filename,
            MA_SOUND_FLAG_STREAM,
            nullptr,
            nullptr,
            &m_sound); r != MA_SUCCESS) {
        throw "Failed to load sound";
    }

    ma_sound_set_looping(&m_sound, looped);
    ma_sound_set_pan(&m_sound, 0.0f);

    m_state = State::PAUSED;

    if (!paused) {
        m_state = State::PLAYING;
        ma_sound_start(&m_sound);
    }
#ifndef NDEBUG
    m_filename = filename;
    g_soundList.push_back(this);
#endif
}

ISound::~ISound()
{
    ma_sound_uninit(&m_sound);
#ifndef NDEBUG
    g_soundList.remove(this);
#endif
}

void ISound::setIsLooped(const bool looped)
{
    ma_sound_set_looping(&m_sound, looped);	
}

void ISound::setIsPaused(const bool paused)
{
    switch(m_state) {
    case State::PAUSED:
        if (!paused) {
            ma_sound_start(&m_sound);
            m_state = State::PLAYING;
        }
        break;
    case State::PLAYING:
        if (paused) {
            ma_sound_stop_with_fade_in_milliseconds(&m_sound, FADEOUT);
            m_state = State::PAUSED;
        }
        break;
    case State::STOPPED:
        oapiWriteLog("XRSound: trying to pause a stopped sound");
        break;
    }
}

void ISound::setPan(const float pan)
{
    // -1.0 = left, 0 = center, +1.0 = right
    ma_sound_set_pan(&m_sound, pan);
}

bool ISound::setPlayPosition(const unsigned int pos)
{
    const ma_uint64 sampleRate = ma_engine_get_sample_rate(ma_sound_get_engine(&m_sound));
    const ma_uint64 frame = (sampleRate * pos) / 1000;
    return ma_sound_seek_to_pcm_frame(&m_sound, frame) == MA_SUCCESS;
}

unsigned int ISound::getPlayPosition() const
{
    ma_uint64 frame;
    ma_sound_get_cursor_in_pcm_frames(&m_sound, &frame);

    const ma_uint64 sampleRate = ma_engine_get_sample_rate(ma_sound_get_engine(&m_sound));

    return static_cast<unsigned int>((frame * 1000) / sampleRate);
}
