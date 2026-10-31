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
#include <string>
#include <unordered_set>

// The "Audio debug" ImGui window uses Orbiter's ImGui dialog API, which Orbiter added after the
// 2024 release; an XRSound.dll that contains it does not load in Orbiter 2024. CMake defines
// XRSOUND_DEBUG_GUI for Debug builds only (configure with -DXRSOUND_DEBUG_GUI=OFF to leave it out
// of a Debug build too). The window must never ship in a Release build.
#if defined(XRSOUND_DEBUG_GUI) && defined(NDEBUG)
#error "XRSOUND_DEBUG_GUI is for Debug builds only"
#endif

#ifdef XRSOUND_DEBUG_GUI
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
	// Sets the volume given to the audio library; returns true if it differs from the volume set last (always true the first time).
	bool setVolume(const float vol) {
		const bool bChanged = (vol != m_volume);
		m_volume = vol;
		ma_sound_set_volume(&m_sound, vol);
		return bChanged;
	}
	// Returns true once setVolume has been called
	bool hasVolume() const { return m_volume >= 0; }
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

#ifdef XRSOUND_DEBUG_GUI
	std::string m_filename;
#endif
	State m_state;
	ma_sound m_sound;
	float m_volume = -1.0f;     // the volume set last by setVolume; -1 until the first call
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

	// Obtains the length of a sound file in milliseconds without playing it (opens a decoder, reads the length, and closes it).
	// Note: for some compressed formats (e.g., a long .mp3) miniaudio may have to scan the file, so this can take a moment.
	// Returns true and sets lengthMillis on success, false if the file could not be decoded or reports an unknown or zero length (lengthMillis is unchanged).
	bool getSoundLengthMillis(const char *soundFileName, unsigned int &lengthMillis) const;
private:
    ma_engine m_engine;
	bool m_initialized = false;
	std::list<ISound *> releasedSounds;
	void CheckReleased();

	// Writes a sound file error to Orbiter.log the first time it happens for that message and file in this session only.
	// XRSound tries to start a sound again on every update while it should be playing, so a broken file would otherwise log the same line 20 times a second.
	void LogFileErrorOnce(const char *pMessage, const char *soundFileName) const;
	mutable std::unordered_set<std::string> m_loggedFileErrors;     // "<message><file name>" for each error already logged
#ifdef XRSOUND_DEBUG_GUI
	ISoundDialog *m_dbgWin;
	DWORD m_dwdbgWin;
#endif
};
