// ==============================================================
// XRSoundEngine10.h : Defines the XRSoundEngine version 1.x interface.
//
// Copyright (c) 2017-2021 Douglas Beachy
// Licensed under the MIT License
// ==============================================================

#pragma once

#include "XRSound.h"   // for enum types; names should never change between versions anyway

// NOTE: do not change the order of these methods!  We need to maintain compatibility with 
// previous versions of the static client library (XRSound.lib) that uses this engine object.

class XRSoundEngine10
{
public:
    virtual float GetVersion() const = 0;
    virtual bool LoadWav(const int soundID, const char *pSoundFilename, const XRSound::PlaybackType playbackType) = 0;
    virtual bool PlayWav(const int soundID, const bool bLoop = false, const float volume = 1.0) = 0;
    virtual bool StopWav(const int soundID) = 0;
    virtual bool IsWavPlaying(const int soundID) = 0;   // can't be const
    virtual bool SetPaused(const int soundID, const bool bPause) = 0;
    virtual bool IsPaused(const int soundID) = 0;  // can't be const

    virtual bool SetDefaultSoundEnabled(const XRSound::DefaultSoundID soundID, const bool bEnabled) = 0;
    virtual bool GetDefaultSoundEnabled(const XRSound::DefaultSoundID soundID) = 0;  // can't be const

    virtual bool SetDefaultSoundGroupFolder(const XRSound::DefaultSoundID groupSoundID, const char *pSubfolderPath) = 0;
    virtual const char *GetDefaultSoundGroupFolder(const XRSound::DefaultSoundID groupSoundID) const = 0;

    // NOTE: these three methods are not part of the public API; however, they are here so that the XRSoundImpl side can call them
    virtual bool IsDefaultSound(const int soundID) const = 0;
    virtual bool IsDefaultSoundGroup(const int soundID) const = 0;
    virtual bool IsDefaultSoundOrGroup(const int soundID) const = 0;
};
