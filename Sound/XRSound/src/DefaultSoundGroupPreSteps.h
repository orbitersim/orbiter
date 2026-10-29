// ==============================================================
// Defines the default group sound handlers for XRSound.
// 
// Copyright (c) 2018-2026 Douglas Beachy and contributors
// Licensed under the MIT License
// ==============================================================

#pragma once

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
    // Returns true if initialization succeeds, false if the group is disabled (empty folder) or the folder scan fails. Either way this prestep stays registered
    // and silent until SetFolder succeeds.
    virtual bool Initialize(const int soundID, const char *pFolderSubpath, const XRSound::PlaybackType playbackType) override;

    // A group has exactly one sound slot, so it can be found by sound ID (see VesselXRSoundEngine::FindDefaultSoundGroupPreStep).
    virtual int GetSoundID() const override { return m_soundID; }

    // Scans pFolderSubpath and, only if the scan succeeds and finds at least one supported file, makes it the active folder; on failure the previous folder and file list remain in effect.
    bool SetFolder(const char *pFolderSubpath);
    const char *GetFolder() const { return m_csFolderSubpath.c_str(); }   // empty if no folder has been set (the group is disabled)

    // Returns true if this group has a usable list of sound files; if false, the group is disabled and its clbkPreStep must stay silent and inactive.
    bool HasSoundFiles() const { return (m_pSoundFilesList && !m_pSoundFilesList->IsEmpty()); }
    std::string GetRandomSoundFile();
    std::string GetNextSoundFile();
    bool LoadWavWithBasename(const char *pBasename);
    bool LoadAndPlayWavWithBasename(const char *pBasename, const bool bLoop = false, const float volume = 1.0f);

protected:
    FileList *m_pSoundFilesList;      // contains list of all files for this group of sounds
    std::string m_csFolderSubpath;        // Relative to $ORBITER_ROOT; e.g., "XRSound\Default\Cabin Ambience"
    int m_currentSoundFileIndex;      // used by GetNextSoundFile
};

// ------------------------------------------------------------------------

class RandomDefaultSoundGroupPreStep : public DefaultSoundGroupPreStep
{
public:
    RandomDefaultSoundGroupPreStep(VesselXRSoundEngine *pEngine);
    virtual void clbkPreStep(const double simt, const double simdt, const double mjd) override;

    // Schedules the next random sound using the current minimum / maximum delay (measured from now). VesselXRSoundEngine calls this when a delay setting changes at runtime.
    void ResetTimer();

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

    // The engine applies this vessel's configured ATC category level during mixing (see DefaultSoundGroupPreSteps.cpp).
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

