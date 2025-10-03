#pragma once
#include <stdio.h>
//#include <AL/al.h>
//#include <AL/alc.h>
#include <SDL3_mixer/SDL_mixer.h>
#include <unordered_map>
#include <string>

class ISound {
public:
    ISound(MIX_Track *audio, bool looped, bool paused);
    ~ISound();
	bool isFinished() { return m_started && !m_paused && !MIX_TrackPlaying(m_track); }
	void setVolume(float vol) { MIX_SetTrackGain(m_track, vol); }
    void setIsLooped(bool looped);
    void setIsPaused(bool paused);
    void setPan(float pan);
	float getPan() { return m_pan; }
	bool setPlaybackSpeed(float speed) { return MIX_SetTrackFrequencyRatio(m_track, speed); }
	float getPlaybackSpeed() { return MIX_GetTrackFrequencyRatio(m_track); }
    bool setPlayPosition(unsigned int pos);
    unsigned int getPlayPosition();
	void stop() { MIX_StopTrack(m_track, 0);}
private:
	MIX_Track *m_track;
	SDL_PropertiesID m_options;
	float m_pan;
	bool m_paused;
	bool m_started;
};

class ISoundEngine {
public:
    ISoundEngine();
    ~ISoundEngine();
    ISound *play2D(const char *soundFileName, bool playLooped=false, bool startPaused=false, bool track=false);
	const char *getDriverName() { return SDL_GetCurrentAudioDriver(); }
//    ALuint preload(const char *soundFileName);
private:
	MIX_Mixer *m_mixer;
    std::unordered_map<std::string, MIX_Audio *> m_buffers;
};
