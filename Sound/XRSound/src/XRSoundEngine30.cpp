// ==============================================================
// XRSound engine implementation.
//
// Copyright (c) 2018-2026 Douglas Beachy and contributors
// Licensed under the MIT License
// ==============================================================

#include "XRSoundEngine.h"
#include "ISound.h"

// Functionality added in XRSound version 3.0

// SetPan, SetPlaybackSpeed, and SetPlayPosition on a loaded sound that is not playing store a one-shot value in WavContext and return true. PlayWav applies and clears
// these values when it creates the ISound for the next start. While a sound is playing, the setters apply immediately. Getters return their not-playing sentinels when no sound is playing,
// even if a pending value is stored, so callers can use them to test playback state.
//
// The shared MusicFolder slot is the exception: only the focus vessel may change its pan, speed, or position, and only while a track is playing. Otherwise the setter returns false.
// Pending values are never stored for this slot, so one vessel's settings cannot carry into a track another vessel starts.

// Returns the music slot's ISound if this engine may change it right now (it plays the slot and the track is playing or paused), otherwise nullptr.
ISound *XRSoundEngine::GetControllableMusicISound(const char *pCaller)
{
    const bool bPlaysMusicSlot = PlaysMusicSlot();
    ISound *pISound = ((bPlaysMusicSlot && s_pMusicFolderWavContext) ? GetActiveISound(*s_pMusicFolderWavContext) : nullptr);
    if (!pISound)
        VERBOSE_LOG(this, "XRSoundEngine::%s WARNING: MusicFolder can be changed only by the focus vessel while music is playing; ignored (PlaysMusicSlot = %d, music context exists = %d)", pCaller, bPlaysMusicSlot, (s_pMusicFolderWavContext != nullptr));
    return pISound;
}

bool XRSoundEngine::SetPan(const int soundID, const float pan)
{
    if (!IsSoundEngineInitialized() || (pan < -1.0) || (pan > 1.0))
        return false;

    if (soundID == XRSound::MusicFolder)
    {
        ISound *pMusic = GetControllableMusicISound("SetPan");
        if (!pMusic)
            return false;
        pMusic->setPan(-pan);   // miniaudio has pan direction inverted with Orbiter's X coordinate system, so flip it
        return true;
    }

    bool retVal = false;
    WavContext *pContext = FindWavContext(soundID);
    if (pContext)
    {
        ISound *pISound = GetActiveISound(*pContext);
        if (pISound)   // is the sound playing (or paused)?
        {
            // miniaudio has pan direction inverted with Orbiter's X coordinate system, so flip it
            pISound->setPan(-pan);
            retVal = true;
        }
        else
        {
            // not playing: remember it for the next start
            pContext->bHasPendingPan = true;
            pContext->pendingPan = pan;
            VERBOSE_LOG(this, "XRSoundEngine::SetPan: stored pending pan %.2f for %s", pan, pContext->ToStr(false).c_str());
            retVal = true;
        }
    }
    return retVal;
}

float XRSoundEngine::GetPan(const int soundID)
{
    if (!IsSoundEngineInitialized())
        return -100;

    float retVal = -100;
    WavContext *pContext = FindWavContext(soundID);
    if (pContext)
    {
        ISound *pISound = GetActiveISound(*pContext);
        if (pISound)   // is the sound playing (or paused)?
        {
            // miniaudio has pan direction inverted with Orbiter's X coordinate system, so flip it
            retVal = -(pISound->getPan());
        }
    }
    return retVal;
}

bool XRSoundEngine::SetPlaybackSpeed(const int soundID, const float speed)
{
    if (!IsSoundEngineInitialized())
        return false;

    if (soundID == XRSound::MusicFolder)
    {
        ISound *pMusic = GetControllableMusicISound("SetPlaybackSpeed");
        return (pMusic && pMusic->setPlaybackSpeed(speed));
    }

    bool retVal = false;
    WavContext* pContext = FindWavContext(soundID);
    if (pContext)
    {
        ISound *pISound = GetActiveISound(*pContext);
        if (pISound)   // is the sound playing (or paused)?
            retVal = pISound->setPlaybackSpeed(speed);
        else if (speed > 0)    // miniaudio ignores a speed of zero or less, so do not accept one that could never be applied
        {
            // not playing: remember it for the next start
            pContext->bHasPendingSpeed = true;
            pContext->pendingSpeed = speed;
            VERBOSE_LOG(this, "XRSoundEngine::SetPlaybackSpeed: stored pending speed %.2f for %s", speed, pContext->ToStr(false).c_str());
            retVal = true;
        }
        else
            VERBOSE_LOG(this, "XRSoundEngine::SetPlaybackSpeed WARNING: invalid pending speed %.2f for %s; ignored", speed, pContext->ToStr(false).c_str());
    }
    return retVal;
}

float XRSoundEngine::GetPlaybackSpeed(const int soundID)
{
    if (!IsSoundEngineInitialized())
        return 0;

    float retVal = 0;
    WavContext *pContext = FindWavContext(soundID);
    if (pContext)
    {
        ISound* pISound = GetActiveISound(*pContext);
        if (pISound)   // is the sound playing (or paused)?
            retVal = pISound->getPlaybackSpeed();
    }
    return retVal;
}


bool XRSoundEngine::SetPlayPosition(const int soundID, const unsigned int positionMillis)
{
    if (!IsSoundEngineInitialized())
        return false;

    if (soundID == XRSound::MusicFolder)
    {
        ISound *pMusic = GetControllableMusicISound("SetPlayPosition");
        return (pMusic && pMusic->setPlayPosition(positionMillis));
    }

    bool retVal = false;
    WavContext* pContext = FindWavContext(soundID);
    if (pContext)
    {
        ISound *pISound = GetActiveISound(*pContext);
        if (pISound)   // is the sound playing (or paused)?
            retVal = pISound->setPlayPosition(positionMillis);
        else if (pContext->bDurationCached && (positionMillis > pContext->cachedDurationMillis))
        {
            // we can only validate the position here if GetDuration has already measured the file; otherwise a bad position is detected (and logged) at start time
            VERBOSE_LOG(this, "XRSoundEngine::SetPlayPosition WARNING: pending position %u ms is beyond the %u ms duration of %s; ignored",
                positionMillis, pContext->cachedDurationMillis, pContext->ToStr(false).c_str());
        }
        else
        {
            // not playing: remember it for the next start
            pContext->bHasPendingPosition = true;
            pContext->pendingPositionMillis = positionMillis;
            VERBOSE_LOG(this, "XRSoundEngine::SetPlayPosition: stored pending position %u ms for %s", positionMillis, pContext->ToStr(false).c_str());
            retVal = true;
        }
    }
    return retVal;
}

int XRSoundEngine::GetPlayPosition(const int soundID)
{
    if (!IsSoundEngineInitialized())
        return -1;

    int retVal = -1;
    WavContext *pContext = FindWavContext(soundID);
    if (pContext)
    {
        ISound* pISound = GetActiveISound(*pContext);
        if (pISound)   // is the sound playing (or paused)?
        {
            // 2 millions seconds is 555 hours, so casting to a signed integer is (quite) sufficent here
            retVal = static_cast<int>(pISound->getPlayPosition());
        }
    }
    return retVal;
}

