// ==============================================================
// XRSoundImpl.h : Defines the XRSound API implementation; this file is not distributed with XRSound.
// 
// Copyright (c) 2018-2021 Douglas Beachy
// Licensed under the MIT License
// ==============================================================

#pragma once

#include <windows.h>

#include "XRSound.h"   
#include "XRSoundEngine.h" 

class XRSoundImpl : public XRSound
{
public:
    XRSoundImpl();
    virtual ~XRSoundImpl();

    // these return true if XRSound.dll is present, false otherwise.  This is NOT a public API call.
    bool Initialize(VESSEL *pVessel);
    bool Initialize(const char *pUniqueModuleName);

    // -------------------------------------------------------------------------------
    // see matching public API methods for documentation for these overridden methods

    // Added in XRSound 1.0
    virtual bool IsPresent() const override { return (m_pEngine != nullptr); }
    virtual float GetVersion() const override;
    virtual bool LoadWav(const int soundID, const char *pSoundFilename, const PlaybackType playbackType) override;
    virtual bool PlayWav(const int soundID, const bool bLoop = false, const float volume = 1.0) override;
    virtual bool StopWav(const int soundID) override;
    virtual bool IsWavPlaying(const int soundID) const override;
    virtual bool SetPaused(const int soundID, const bool bPause) override;
    virtual bool IsPaused(const int soundID) const override;
    virtual bool SetDefaultSoundEnabled(const DefaultSoundID soundID, const bool bEnabled) override;
    virtual bool GetDefaultSoundEnabled(const DefaultSoundID soundID) const override;
    virtual bool SetDefaultSoundGroupFolder(const DefaultSoundID groupSoundID, const char *pSubfolderPath) override;
    virtual const char *GetDefaultSoundGroupFolder(const DefaultSoundID groupSoundID) const override;

    // Added in XRSound 2.0
    XRSoundEngine::EngineType GetEngineType() const;  // calls XRSound.dll
    const char *GetLogID() const;                     // calls XRSound.dll
    bool HaveMinDLLVersion(const float) const;

    // Added in XRSound 3.0
    virtual bool  SetPan(const int soundID, const float pan);
    virtual float GetPan(const int soundID);

    virtual bool  SetPlaybackSpeed(const int soundID, const float speed = 1.0);
    virtual float GetPlaybackSpeed(const int soundID);

    virtual bool  SetPlayPosition(const int soundID, const unsigned int positionMillis);
    virtual int   GetPlayPosition(const int soundID);

    //========================================================================================================================
    // NOTE: methods below here are not part of the public API; however, we call these DLL methods from here on the impl side
    //========================================================================================================================
    bool IsDefaultSound(const int soundID) const;
    bool IsDefaultSoundGroup(const int soundID) const;
    bool IsDefaultSoundOrGroup(const int soundID) const;

    // static methods
    static const char *EngineTypeToStr(const XRSoundEngine::EngineType engineType);

    // -------------------------------------------------------------------------------

private:
    HMODULE m_hDLL;
    XRSoundEngine *m_pEngine;   // created by XRSound.dll; this is a BORROWED reference; do not free it from this side!
};