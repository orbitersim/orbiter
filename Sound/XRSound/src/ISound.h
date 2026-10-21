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

#ifndef NDEBUG
#include <string>
#endif

#define FADEOUT 75

class ISound {
public:
	enum class State {
		PAUSED,
		PLAYING,
		STOPPED
	};

	ISound(ma_engine* engine,
           const char* filename,
           const bool looped,
           const bool paused);
	~ISound();

	// We will garbage collect an ISound object in 2 cases :
	// - stop() was called and the fade out is finished
	// - the sound has played to its end
	bool isFinished() const { return m_state != State::PAUSED && !ma_sound_is_playing(&m_sound); }
	void setVolume(const float vol) {
		// XRSound usually creates a sound paused, then sets its volume according to its PlaybackType.
		// We don't want any fading in that case.
		if (m_state == State::PLAYING)
			ma_sound_set_fade_in_milliseconds(&m_sound, -1, vol, 5);
		else
			ma_sound_set_volume(&m_sound, vol);
	}
    void setIsLooped(const bool looped);
    void setIsPaused(const bool paused);
    void setPan(const float pan);
	float getPan() const { return ma_sound_get_pan(&m_sound); }
	bool setPlaybackSpeed(const float speed) { ma_sound_set_pitch(&m_sound, speed); return true; }
	float getPlaybackSpeed() const { return ma_sound_get_pitch(&m_sound); }
    bool setPlayPosition(const unsigned int pos);
    unsigned int getPlayPosition() const;
	void stop() { 
		if(m_state == State::PLAYING)
			ma_sound_stop_with_fade_in_milliseconds(&m_sound, FADEOUT);
		m_state = State::STOPPED;
	}

#ifndef NDEBUG
	std::string m_filename;
#endif
	State m_state;
	ma_sound m_sound;
};

// Basic sound engine
// It must be instanciated only once
class ISoundDialog;
class ISoundEngine {
public:
    ISoundEngine();
    ~ISoundEngine();
    ISound *play2D(const char *soundFileName, const bool playLooped=false, const bool startPaused=false, const bool track=false);
	void Release(ISound *);
	const char *getDriverName() { return "miniaudio"; }
private:
    ma_engine m_engine;
	bool m_initialized = false;
	std::list<ISound *> releasedSounds;
	void CheckReleased();
#ifndef NDEBUG
	ISoundDialog *m_dbgWin;
	DWORD m_dwdbgWin;
#endif
};
