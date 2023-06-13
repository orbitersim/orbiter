// ==============================================================
// Defines the default group sound handlers for XRSound.
// 
// Copyright (c) 2018-2021 Douglas Beachy
// Licensed under the MIT License
// ==============================================================

#pragma once

#include <atlstr.h>             // for CString
#include <unordered_map>

#include "OrbiterSDK.h"
#include "XRSound.h"            // for enums
#include "VesselXRSoundEngine.h"
#include "SoundPreSteps.h"
#include "FileList.h"

using namespace std;

// Abstract base class that handles playing back a default sound from a group of sounds
class DefaultSoundGroupPreStep : public DefaultSoundPreStep
{
public:
    DefaultSoundGroupPreStep(VesselXRSoundEngine *pEngine);
    virtual ~DefaultSoundGroupPreStep();
    virtual bool Initialize(const int soundID, const char *pFolderSubpath, const XRSound::PlaybackType playbackType) override;
    bool SetFolder(const char *pFolderSubpath);
    const char *GetFolder() const { return m_csFolderSubpath; }
    CString GetRandomSoundFile();
    CString GetNextSoundFile();
    bool LoadWavWithBasename(const char *pBasename);
    bool LoadAndPlayWavWithBasename(const char *pBasename, const bool bLoop = false, const float volume = 1.0f);

protected:
    FileList *m_pSoundFilesList;      // contains list of all files for this group of sounds
    CString m_csFolderSubpath;        // Relative to $ORBITER_ROOT; e.g., "XRSound\Default\Cabin Ambience"
    int m_currentSoundFileIndex;      // used by GetNextSoundFile
};

// ------------------------------------------------------------------------

class RandomDefaultSoundGroupPreStep : public DefaultSoundGroupPreStep
{
public:
    RandomDefaultSoundGroupPreStep(VesselXRSoundEngine *pEngine);
    virtual void clbkPreStep(const double simt, const double simdt, const double mjd) override;

protected:
    // subclasses should return true if sound is OK to play right now (invoked when next sound is due to play)
    virtual bool ShouldPlayNow(const double simt, const double simdt, const double mjd) = 0;

    // Holds minimum and maximum delay for next playback in seconds.
    // These values are doubles for ease of use in the class logic.
    struct MinMaxDelay
    {
        MinMaxDelay(const double min, const double max) : Min(min), Max(max) { }
        double Min;
        double Max;
    };

    virtual MinMaxDelay GetMinMaxDelay() = 0;

    void PlayRandom();
    void ResetTimer();

    double m_nextPlayTime;  // this is in *realitime*, not simt
};

// ------------------------------------------------------------------------

class AmbientDefaultSoundGroupPreStep : public RandomDefaultSoundGroupPreStep
{
public:
    AmbientDefaultSoundGroupPreStep(VesselXRSoundEngine *pEngine);

protected:
    virtual bool ShouldPlayNow(const double simt, const double simdt, const double mjd) override;
    virtual MinMaxDelay GetMinMaxDelay() override;
};


class ATCDefaultSoundGroupPreStep : public RandomDefaultSoundGroupPreStep
{
public:
    ATCDefaultSoundGroupPreStep(VesselXRSoundEngine *pEngine);

protected:
    virtual bool ShouldPlayNow(const double simt, const double simdt, const double mjd) override;
    virtual MinMaxDelay GetMinMaxDelay() override;

    virtual bool PlayWav(const bool bLoop, float volume = 1.0) override;
};

class AltitudeCalloutsDefaultSoundGroupPreStep : public DefaultSoundGroupPreStep
{
public:
    AltitudeCalloutsDefaultSoundGroupPreStep(VesselXRSoundEngine *pEngine);

    // this is invoked at every timestep
    virtual void clbkPreStep(const double simt, const double simdt, const double mjd);

protected:
    double m_previousFrameAltitude;
    double m_nextMinimumCalloutTime;

    void SetNextMinimumCalloutTime(const double simt)
    {
        m_nextMinimumCalloutTime = simt + 1;    // minimum delay is one second
    }
};

class DockingCalloutsDefaultSoundGroupPreStep : public DefaultSoundGroupPreStep
{
public:
    DockingCalloutsDefaultSoundGroupPreStep(VesselXRSoundEngine *pEngine);

    // this is invoked at every timestep
    virtual void clbkPreStep(const double simt, const double simdt, const double mjd);

protected:
    double m_previousFrameDistance;
    double m_nextMinimumCalloutTime;
    double m_previousSimt;
    double m_undockingMsgTime;
    bool m_bJustDocked;
    bool m_previousWasDocked;
    double m_intervalStartTime;
    double m_intervalStartDistance;

    void SetNextMinimumCalloutTime(const double simt)
    {
        m_nextMinimumCalloutTime = simt + 1;    // minimum delay is one second
    }
};

class MachCalloutsDefaultSoundGroupPreStep : public DefaultSoundGroupPreStep
{
public:
    MachCalloutsDefaultSoundGroupPreStep(VesselXRSoundEngine *pEngine);
    void clbkPreStep(const double simt, const double simdt, const double mjd);

protected:
    double m_previousMach;
    double m_nextMinimumCalloutTime;
};

class MusicDefaultSoundGroupPreStep : public DefaultSoundGroupPreStep
{
public:
    MusicDefaultSoundGroupPreStep(VesselXRSoundEngine *pEngine);
    virtual void clbkPreStep(const double simt, const double simdt, const double mjd) override;

protected:
    double m_startNextSongRealtime;  // -1 = "not set yet"
};
