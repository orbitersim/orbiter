#include "ISound.h"

ISoundEngine::ISoundEngine()
{
	if(!MIX_Init()) {
		exit(1);
	}
	m_mixer = MIX_CreateMixerDevice(SDL_AUDIO_DEVICE_DEFAULT_PLAYBACK, NULL);

	if(!m_mixer) {
		exit(2);
	}
}

ISoundEngine::~ISoundEngine()
{
	MIX_DestroyMixer(m_mixer);
	MIX_Quit();
}

ISound *ISoundEngine::play2D(const char * soundFileName, bool playLooped, bool startPaused, bool track)
{
	MIX_Audio *audio;

	auto v = m_buffers.find(soundFileName);

	if(v == m_buffers.end()) {
        printf("ISoundEngine::play2D: late loading of %s\n", soundFileName);
        audio = MIX_LoadAudio(m_mixer, soundFileName, false);
		m_buffers.insert({soundFileName, audio});
    } else {
        printf("ISoundEngine::play2D: using cache of %s\n", soundFileName);
        audio = v->second;
    }

    if(audio == nullptr) {
        fprintf(stderr,"failed to create buffer for %s\n", soundFileName);
        exit(EXIT_FAILURE);
    }

	MIX_Track *audio_track = MIX_CreateTrack(m_mixer);
	if(!MIX_SetTrackAudio(audio_track, audio))
		exit(EXIT_FAILURE);

    ISound *snd = new ISound(audio_track, playLooped, startPaused);

    if(startPaused || track)
        return snd;
    else
        return nullptr;
}

ISound::ISound(MIX_Track *track, bool looped, bool paused)
{
	m_started = !paused;
	m_paused = paused;
	m_pan = 0;
	m_track = track;
	m_options = SDL_CreateProperties();
    SDL_SetNumberProperty(m_options, MIX_PROP_PLAY_LOOPS_NUMBER, looped ? -1 : 0);
	if(!paused)
	    MIX_PlayTrack(m_track, m_options);
}

ISound::~ISound()
{
	if(MIX_TrackPlaying(m_track)) {
		MIX_StopTrack(m_track, 0);
	}
    SDL_DestroyProperties(m_options);
	MIX_DestroyTrack(m_track);
}

void ISound::setIsLooped(bool looped)
{
	
}

void ISound::setIsPaused(bool paused)
{
	if(paused)
		MIX_PauseTrack(m_track);
	else {
		if(m_started)
			MIX_ResumeTrack(m_track);
		else {
			m_started = true;
			MIX_PlayTrack(m_track, m_options);
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
