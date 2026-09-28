// ==============================================================
// XRSound engine implementation.
// 
// Copyright (c) 2018-2026 Douglas Beachy
// Licensed under the MIT License
// ==============================================================

#include "XRSoundEngine.h"

// Functionality added in XRSound version 3.0

bool XRSoundEngine::SetPan(const int soundID, const float pan)
{
    if (!IsKlangEngineInitialized() || (pan < -1.0) || (pan > 1.0))
        return false;

    bool retVal = false;
    WavContext *pContext = FindWavContext(soundID);
    if (pContext)
    {
#ifndef __linux__
        ISound *pISound = pContext->pISound;
#else // __linux__
        AudioVoice *pISound = pContext->pISound;
#endif // __linux__
        if (pISound)   // was sound ever started via PlayWav?
        {
            // irrKlang has pan direction inverted with Orbiter's X coordinate system, so flip it
#ifndef __linux__
            pISound->setPan(-pan);
#else // __linux__
            // our engine pans -1 left .. 1 right like this API, so there is nothing to flip
            pISound->SetPan(pan);
#endif // __linux__
            retVal = true;
        }
    }
    return retVal;
}

float XRSoundEngine::GetPan(const int soundID)
{
    if (!IsKlangEngineInitialized())
        return -100;

    float retVal = -100;
    WavContext *pContext = FindWavContext(soundID);
    if (pContext)
    {
#ifndef __linux__
        ISound *pISound = pContext->pISound;
#else // __linux__
        AudioVoice *pISound = pContext->pISound;
#endif // __linux__
        if (pISound)   // was sound ever started via PlayWav?
        {
            // irrKlang has pan direction inverted with Orbiter's X coordinate system, so flip it
#ifndef __linux__
            retVal = -(pISound->getPan());
#else // __linux__
            // our engine pans -1 left .. 1 right like this API, so there is nothing to flip
            retVal = pISound->GetPan();
#endif // __linux__
        }
    }
    return retVal;
}

bool XRSoundEngine::SetPlaybackSpeed(const int soundID, const float speed)
{
    if (!IsKlangEngineInitialized())
        return false;

    bool retVal = false;
    WavContext* pContext = FindWavContext(soundID);
    if (pContext)
    {
#ifndef __linux__
        ISound *pISound = pContext->pISound;
#else // __linux__
        AudioVoice *pISound = pContext->pISound;
#endif // __linux__
        if (pISound)   // was sound ever started via PlayWav?
#ifndef __linux__
            retVal = pISound->setPlaybackSpeed(speed);
#else // __linux__
            retVal = pISound->SetPlaybackSpeed(speed);
#endif // __linux__
    }
    return retVal;
}

float XRSoundEngine::GetPlaybackSpeed(const int soundID)
{
    if (!IsKlangEngineInitialized())
        return 0;

    float retVal = 0;
    WavContext *pContext = FindWavContext(soundID);
    if (pContext)
    {
#ifndef __linux__
        ISound* pISound = pContext->pISound;
#else // __linux__
        AudioVoice* pISound = pContext->pISound;
#endif // __linux__
        if (pISound)   // was sound ever started via PlayWav?
#ifndef __linux__
            retVal = pISound->getPlaybackSpeed();
#else // __linux__
            retVal = pISound->GetPlaybackSpeed();
#endif // __linux__
    }
    return retVal;
}


bool XRSoundEngine::SetPlayPosition(const int soundID, const unsigned int positionMillis)
{
    if (!IsKlangEngineInitialized())
        return false;

    bool retVal = false;
    WavContext* pContext = FindWavContext(soundID);
    if (pContext)
    {
#ifndef __linux__
        ISound *pISound = pContext->pISound;
#else // __linux__
        AudioVoice *pISound = pContext->pISound;
#endif // __linux__
        if (pISound)   // was sound ever started via PlayWav?
#ifndef __linux__
            retVal = pISound->setPlayPosition(positionMillis);
#else // __linux__
            retVal = pISound->SetPlayPosition(positionMillis);
#endif // __linux__
    }
    return retVal;
}

int XRSoundEngine::GetPlayPosition(const int soundID)
{
    if (!IsKlangEngineInitialized())
        return -1;

    int retVal = -1;
    WavContext *pContext = FindWavContext(soundID);
    if (pContext)
    {
#ifndef __linux__
        ISound* pISound = pContext->pISound;
#else // __linux__
        AudioVoice* pISound = pContext->pISound;
#endif // __linux__
        if (pISound)   // was sound ever started via PlayWav?
        {
            // 2 millions seconds is 555 hours, so casting to a signed integer is (quite) sufficent here
#ifndef __linux__
            retVal = static_cast<int>(pISound->getPlayPosition());
#else // __linux__
            retVal = static_cast<int>(pISound->GetPlayPosition());
#endif // __linux__
        }
    }
    return retVal;
}

