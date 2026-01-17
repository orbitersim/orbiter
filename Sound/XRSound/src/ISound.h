// ==============================================================
// miniaudio implementation of the ISound API
// 
// Copyright (c) 2025 Gondos
// Licensed under the MIT License
// ==============================================================

#pragma once
#include <stdio.h>
#include "miniaudio.h"
#include <list>

#define FADEOUT 75

class ISound {
public:
    ISound(ma_engine* engine,
           const char* filename,
           bool looped,
           bool paused);
	~ISound();
	bool isFinished() { return m_started && !m_paused && !ma_sound_is_playing(&m_sound); }
	void setVolume(float vol) { ma_sound_set_fade_in_milliseconds(&m_sound, -1, vol, 5); }
    void setIsLooped(bool looped);
    void setIsPaused(bool paused);
    void setPan(float pan);
	float getPan() { return m_pan; }
	bool setPlaybackSpeed(float speed) { ma_sound_set_pitch(&m_sound, speed); return true; }
	float getPlaybackSpeed() { return ma_sound_get_pitch(&m_sound); }
    bool setPlayPosition(unsigned int pos);
    unsigned int getPlayPosition();
	void stop() { ma_sound_stop_with_fade_in_milliseconds(&m_sound, FADEOUT); }
private:
	ma_sound m_sound;
	float m_pan;
	bool m_paused;
	bool m_started;
};

// Basic sound engine
// It must be instanciated only once
class ISoundEngine {
public:
    ISoundEngine();
    ~ISoundEngine();
    ISound *play2D(const char *soundFileName, bool playLooped=false, bool startPaused=false, bool track=false);
	void Release(ISound *);
	const char *getDriverName() { return "miniaudio"; }
private:
    ma_engine m_engine;
	bool m_initialized = false;
	std::list<ISound *> releasedSounds;
	void CheckReleased();
};
