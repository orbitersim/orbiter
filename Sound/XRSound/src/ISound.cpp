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

ISoundEngine::ISoundEngine()
{
    if (ma_engine_init(nullptr, &m_engine) != MA_SUCCESS) {
        //oapiAddNotification(OAPINOTIF_ERROR, "Error initialising miniaudio engine");
		return;
    }
	m_initialized = true;
}

ISoundEngine::~ISoundEngine()
{
	if(!m_initialized) return;

	for(auto &snd: releasedSounds) {
		delete snd;
	}
    ma_engine_uninit(&m_engine);
}

ISound *ISoundEngine::play2D(const char *soundFileName, bool playLooped, bool startPaused, bool track)
{
	if(!m_initialized) return nullptr;

	CheckReleased();
	ISound *snd = nullptr;
	try {
		snd = new ISound(&m_engine, soundFileName, playLooped, startPaused);
	}
	catch(...) {
		//oapiAddNotification(OAPINOTIF_ERROR, "Error loading sound file", soundFileName);
		return nullptr;
	}

    if (startPaused || track)
        return snd;

	Release(snd);
    return nullptr;
}

void ISoundEngine::Release(ISound *snd)
{
	if(!m_initialized) return;
	if(!snd) return;

	CheckReleased();
	if(snd->isFinished())
		delete snd;
	else
		releasedSounds.push_back(snd);
}

void ISoundEngine::CheckReleased()
{
	for(auto it = releasedSounds.begin(); it != releasedSounds.end();) {
		if((*it)->isFinished()) {
			delete *it;
			it = releasedSounds.erase(it);
		} else {
			++it;
		}
	}
}


ISound::ISound(ma_engine* engine,
               const char* filename,
               bool looped,
               bool paused)
{
    if (ma_result r = ma_sound_init_from_file(
            engine,
            filename,
            MA_SOUND_FLAG_STREAM,
            nullptr,
            nullptr,
            &m_sound); r != MA_SUCCESS)
    {
        throw "Failed to load sound";
    }

    ma_sound_set_looping(&m_sound, looped);
    ma_sound_set_pan(&m_sound, 0.0f);

    m_started = !paused;
	m_paused = paused;

    if (!paused)
        ma_sound_start(&m_sound);
}

ISound::~ISound()
{
    ma_sound_uninit(&m_sound);
}

void ISound::setIsLooped(bool looped)
{
    ma_sound_set_looping(&m_sound, looped);	
}

void ISound::setIsPaused(bool paused)
{
    if (paused) {
         ma_sound_stop_with_fade_in_milliseconds(&m_sound, FADEOUT);
    } else {
        if (!m_started)
            m_started = true;

        ma_sound_start(&m_sound);
    }
	m_paused = paused;
}

void ISound::setPan(float pan)
{
    // -1.0 = left, 0 = center, +1.0 = right
    ma_sound_set_pan(&m_sound, pan);
	m_pan = pan;
}

bool ISound::setPlayPosition(unsigned int pos)
{
    ma_uint64 sampleRate = ma_engine_get_sample_rate(
        ma_sound_get_engine(&m_sound));

    ma_uint64 frame = (sampleRate * pos) / 1000;
    return ma_sound_seek_to_pcm_frame(&m_sound, frame) == MA_SUCCESS;
}

unsigned int ISound::getPlayPosition()
{
    ma_uint64 frame;
    ma_sound_get_cursor_in_pcm_frames(&m_sound, &frame);

    ma_uint64 sampleRate = ma_engine_get_sample_rate(
        ma_sound_get_engine(&m_sound));

    return static_cast<unsigned int>((frame * 1000) / sampleRate);
}
