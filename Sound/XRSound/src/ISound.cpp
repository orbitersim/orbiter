// ==============================================================
// SDL3_mixer implementation of the ISound API
// 
// Copyright (c) 2025 Gondos
// Licensed under the MIT License
// ==============================================================

#include "ISound.h"
#include "OrbiterAPI.h"

ISoundEngine::ISoundEngine()
{
	if(!MIX_Init()) {
		oapiWriteLogError("SDL3_mixer: Could not initialize mixer: %s", SDL_GetError());
		m_mixer = nullptr;
		return;
	}
	m_mixer = MIX_CreateMixerDevice(SDL_AUDIO_DEVICE_DEFAULT_PLAYBACK, NULL);

	if(!m_mixer) {
		oapiWriteLogError("SDL3_mixer: Could not initialize mixer device: %s", SDL_GetError());
	}

	// The sounds fell saturated so we lower the master gain
	MIX_SetMasterGain(m_mixer, 0.7f);
}

ISoundEngine::~ISoundEngine()
{
	for(auto &audio: m_buffers) {
		MIX_DestroyAudio(audio.second);
	}
	MIX_DestroyMixer(m_mixer);
	MIX_Quit();
}

// Create an audio track, attach audio data to it and return a corresponding ISound object
ISound *ISoundEngine::play2D(const char * soundFileName, bool playLooped, bool startPaused)
{
	// The core handles null pointers so we bail out early if the initialization failed
	if(!m_mixer) return nullptr;

	MIX_Audio *audio;

	// Check if the sound file is alread in the cache
	auto v = m_buffers.find(soundFileName);
	if(v == m_buffers.end()) {
        //printf("ISoundEngine::play2D: late loading of %s\n", soundFileName);
        audio = MIX_LoadAudio(m_mixer, soundFileName, false);
		if(audio) {
			m_buffers.insert({soundFileName, audio});
		} else {
			oapiWriteLogError("SDL3_mixer: Failed to load sound file %s: %s", soundFileName, SDL_GetError());
			return nullptr;
		}
    } else {
        //printf("ISoundEngine::play2D: using cache of %s\n", soundFileName);
        audio = v->second;
    }

	MIX_Track *audio_track = MIX_CreateTrack(m_mixer);
	if(!audio_track) {
		oapiWriteLogError("SDL3_mixer: Failed to create track: %s", SDL_GetError());
		return nullptr;
	}

	if(!MIX_SetTrackAudio(audio_track, audio)) {
		MIX_DestroyTrack(audio_track);
		oapiWriteLogError("SDL3_mixer: Failed to set track audio: %s", SDL_GetError());
		return nullptr;
	}

    return new ISound(audio_track, playLooped, startPaused);
}

ISound::ISound(MIX_Track *track, bool looped, bool paused)
{
	m_started = !paused;
	m_paused = paused;
	m_pan = 0;
	m_track = track;
	m_options = SDL_CreateProperties();
    SDL_SetNumberProperty(m_options, MIX_PROP_PLAY_LOOPS_NUMBER, looped ? -1 : 0);
	if(!paused) {
		if(!MIX_PlayTrack(m_track, m_options)) {
			oapiWriteLogError("SDL3_mixer: Failed to play track: %s", SDL_GetError());
		}
	}
}

ISound::~ISound()
{
	// The doc says that destroying a track that is playing in another thread
	// will block so we stop it there. Is it enough?
	if(MIX_TrackPlaying(m_track)) {
		MIX_StopTrack(m_track, 0);
	}
    SDL_DestroyProperties(m_options);
	MIX_DestroyTrack(m_track);
}

void ISound::setIsLooped(bool looped)
{
	// With SDL3_mixer, changing the loops number will only take effects if the track is stopped and restarted
	// We don't want that so we only update the m_options member and hope that the user has not started the playback yet
	SDL_SetNumberProperty(m_options, MIX_PROP_PLAY_LOOPS_NUMBER, looped ? -1 : 0);
	if(m_started) {
		// Drop a one time warning if that's not the case
		static std::once_flag once;
		std::call_once(once, []{
			oapiWriteLogError("SDL3_mixer: Changing looping status of a playing sound is unsupported");
		});
	}
}

void ISound::setIsPaused(bool paused)
{
	if(paused) {
		MIX_PauseTrack(m_track);
	} else {
		// We cannot start a track in a paused state
		// so we use m_started to check if the track
		// needs to be started or resumed
		if(m_started) {
			MIX_ResumeTrack(m_track);
		} else {
			m_started = true;
			if(!MIX_PlayTrack(m_track, m_options)) {
				oapiWriteLogError("SDL3_mixer: Failed to play track: %s", SDL_GetError());
			}
		}
	}

	m_paused = paused;
}

void ISound::setPan(float pan)
{
	m_pan = pan;
    const float right = (pan + 1.0f) / 2.0f;
    const MIX_StereoGains gains = { 1.0f - right, right };
	MIX_SetTrackStereo(m_track, &gains);
}

bool ISound::setPlayPosition(unsigned int pos)
{
	Sint64 frames = MIX_TrackMSToFrames(m_track, pos);
	return MIX_SetTrackPlaybackPosition(m_track, frames);
}

unsigned int ISound::getPlayPosition()
{
	Sint64 frames = MIX_GetTrackPlaybackPosition(m_track);
	return MIX_TrackFramesToMS(m_track, frames);
}
