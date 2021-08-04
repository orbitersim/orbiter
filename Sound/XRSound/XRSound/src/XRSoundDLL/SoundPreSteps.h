// ==============================================================
// Defines the default sounds for XRSound.
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

using namespace std;

// Abstract base class for all our sound PreStep objects
class SoundPreStep
{
public:
    SoundPreStep(VesselXRSoundEngine *pEngine) : m_pEngine(pEngine)
    { 
        _ASSERTE(pEngine); 
    }

    XRSoundConfigFileParser &GetConfig()
    {
        _ASSERTE(m_pEngine);
        return m_pEngine->GetConfig();
    }

    bool HasFocus() const
    {
        _ASSERTE(m_pEngine);
        return m_pEngine->HasFocus();
    }

    bool InCockpitView() const
    {
        _ASSERTE(m_pEngine);
        return m_pEngine->InCockpitView();
    }

    void WriteLog(const char *pMsg)
    {
        _ASSERTE(m_pEngine);
        m_pEngine->WriteLog(pMsg);
    }
    
    VESSEL *GetVessel()
    {
        _ASSERTE(m_pEngine);
        return m_pEngine->GetVessel();
    }

    virtual int GetSoundID() const { return -1; }     // subclasses that use ONLY a single default sound ID, AND NO OTHERS, should override this
    virtual void clbkPreStep(const double simt, const double simdt, const double mjd) = 0;

protected:
    bool PlaySwitch(const bool bOn, const float volume = 1.0);
    bool LoadWav(const int soundID, const char *pSoundFilename, const XRSound::PlaybackType playbackType);
    bool PlayWav(const int soundID, const bool bLoop = false, const float volume = 1.0);
    bool LoadAndPlayWavUsingID(const int soundID, const char *pWavFile, const bool bLoop, const XRSound::PlaybackType playbackType, const float volume = 1.0);
    bool StopWav(const int soundID);

    VesselXRSoundEngine *m_pEngine;
};

// Abstract base class defining a callback method for playing back a single default sound, such as air conditioning, etc.
class DefaultSoundPreStep : public SoundPreStep
{
public:
    DefaultSoundPreStep(VesselXRSoundEngine *pEngine);
    virtual ~DefaultSoundPreStep();

    bool IsInitialized() const { return (m_soundID > 0); }
    // Do not override 'GetSoundID()': some subclasses use more than a single sound ID, so we cannot reliably search for them

    // Default implementation; if you override this, make sure to set m_soundID from soundID.
    // Returns true if initialization successful, false if sound is disabled or file load failed (the sound will not play)
    virtual bool Initialize(const int soundID, const char *pSoundFilename, const XRSound::PlaybackType playbackType);

protected:
    int m_soundID;
    XRSound::PlaybackType m_playbackType;
    bool m_bWavPresent;

    // this is invoked at every timestep; subclasses should decide when to play or stop their sound
    virtual void clbkPreStep(const double simt, const double simdt, const double mjd) = 0;

    // methods that act on our m_soundID
    bool LoadWav(const char *pSoundFilename);
    bool LoadAndPlayWav(const char *pSoundFilename, float volume = 1.0); // plays non-looped
    virtual bool PlayWav(const bool bLoop = false, float volume = 1.0);  // virtual so child subclasses can hook it and override the volume set by superclass call
    bool StopWav();
    bool IsWavPlaying() const;
    bool SetPaused(const bool bPause);
    bool IsPaused() const;
};

// PreStep that tracks vessel thruster data, if configured
class LogThrusterDataPreStep : public SoundPreStep
{
public:
    LogThrusterDataPreStep(VesselXRSoundEngine *pEngine);
    virtual void clbkPreStep(const double simt, const double simdt, const double mjd) override;

protected:
    double m_simtNextLog;
};

// Class handling custom engine thrust sounds (e.g., SCRAM engines)
class CustomEnginesDefaultSoundPreStep : public DefaultSoundPreStep
{
public:
    CustomEnginesDefaultSoundPreStep(VesselXRSoundEngine *pEngine);
    virtual void clbkPreStep(const double simt, const double simdt, const double mjd);

protected:
    bool m_bFirstRun;
};

// Class handling engine thrust sounds (excluding RCS)
class EngineDefaultSoundPreStep : public DefaultSoundPreStep
{
public:
    EngineDefaultSoundPreStep(VesselXRSoundEngine *pEngine, const THGROUP_TYPE thgType, const float minVolume, const float maxVolume);

    // this is invoked at every timestep; subclasses should decide when to play or stop their sound
    virtual void clbkPreStep(const double simt, const double simdt, const double mjd);

protected:
    THGROUP_TYPE m_thgType;
    float m_minVolume;
    float m_maxVolume;
};

// Class handling RCS thrust sounds
class RCSDefaultSoundPreStep : public DefaultSoundPreStep
{
public:
    RCSDefaultSoundPreStep(VesselXRSoundEngine *pEngine);
    virtual ~RCSDefaultSoundPreStep();

    // Returns true if initialization successful, false if sound is disabled or file load failed (the sound will not play)
    virtual bool Initialize(const int soundID, const char *pSoundFilename, const XRSound::PlaybackType playbackType) override;

    // this is invoked at every timestep; subclasses should decide when to play or stop their sound
    virtual void clbkPreStep(const double simt, const double simdt, const double mjd);

protected:
    // Class that handles a single RCS axis's RCS Attack sound
    class RCSAttackForAxisSound
    {
    public:
        RCSAttackForAxisSound(const double &axisThrustLevel, const int soundID, VesselXRSoundEngine *pEngine, const bool bNegativeAxis, const char *pWavFilePath);
        ~RCSAttackForAxisSound();
        void clbkPreStep();

    protected:
        const double &m_axisThrustLevel;
        int m_soundID;
        VesselXRSoundEngine *m_pEngine;
        bool m_bNegativeAxis;         // if true, thrust level is along the negative axis
        bool m_bAttackSoundMayPlay;   // if true, RCS Attack sound should start playing on the next call to clbkPreStep *if* thrust level >= m_minThrustLevelForSound
    };

    // data
    static const float s_minThrustLevelForSound;

    // these two values are refreshed each frame
    VECTOR3 m_thrustVectorsROT;
    VECTOR3 m_thrustVectorsLIN;

    // two axis directions * six axes = 12 total
    RCSAttackForAxisSound *m_pRCSAttackForAxisSoundArray[12]; 
};

// ------------------------------------------------------------------------

// Subclass that handles playing all animation sounds (Open/Moving/Close, etc.) set via properties read from vessel class-override .cfg files.
// These are primarily used to play back default animation sounds, but users may add their own sounds for their own
// animiations simply by editing their vessel's XRSound-<vessel class>.cfg file and using a unique ID.
class AnimationSoundPreStep : public SoundPreStep
{
public:
    AnimationSoundPreStep(VesselXRSoundEngine *pEngine, const AnimationSounds &animationSounds);
    virtual void clbkPreStep(const double simt, const double simdt, const double mjd) override;

protected:
    const AnimationSounds &m_animationSounds;     // from XRSoundConfigFileParser
};

// ------------------------------------------------------------------------

class AirConditioningDefaultSoundPreStep : public DefaultSoundPreStep
{
public:
    AirConditioningDefaultSoundPreStep(VesselXRSoundEngine *pEngine) : DefaultSoundPreStep(pEngine) { }
    virtual void clbkPreStep(const double simt, const double simdt, const double mjd) override;
};

class LandedWindDefaultSoundPreStep : public DefaultSoundPreStep
{
public:
    LandedWindDefaultSoundPreStep(VesselXRSoundEngine *pEngine) : DefaultSoundPreStep(pEngine) { }
    virtual void clbkPreStep(const double simt, const double simdt, const double mjd) override;
};

class AudioGreetingDefaultSoundPreStep : public DefaultSoundPreStep
{
public:
    AudioGreetingDefaultSoundPreStep(VesselXRSoundEngine *pEngine) : DefaultSoundPreStep(pEngine) { }
    virtual void clbkPreStep(const double simt, const double simdt, const double mjd) override;
    
    // this must be public so it can be reset when the simulation restarts
    static bool s_bPlayedGreeting;
};

class RCSModeDefaultSoundPreStep : public SoundPreStep
{
public:
    RCSModeDefaultSoundPreStep(VesselXRSoundEngine *pEngine);
    virtual void clbkPreStep(const double simt, const double simdt, const double mjd) override;

protected:
    int m_previousRCSMode;
};

class AFCtrlModeDefaultSoundPreStep : public SoundPreStep
{
public:
    AFCtrlModeDefaultSoundPreStep(VesselXRSoundEngine *pEngine);
    virtual void clbkPreStep(const double simt, const double simdt, const double mjd) override;

protected:
    int m_previousAFCtrlMode;
};

class TakeoffAndLandingCalloutsAndCrashPreStep : public SoundPreStep
{
public:
    TakeoffAndLandingCalloutsAndCrashPreStep(VesselXRSoundEngine *pEngine);
    virtual void clbkPreStep(const double simt, const double simdt, const double mjd) override;

protected:
    double m_takeoffTime;       // in simt
    double m_previousFrameVerticalSpeed;
    double m_airborneTargetTime;
    double m_touchdownTime;     // in simt
    double m_previousFrameAirspeed;
};

class WheelbrakeDefaultSoundPreStep : public DefaultSoundPreStep
{
public:
    WheelbrakeDefaultSoundPreStep(VesselXRSoundEngine *pEngine) : DefaultSoundPreStep(pEngine) { }
    virtual void clbkPreStep(const double simt, const double simdt, const double mjd) override;
};

class FlightWindAndPlasmaSoundPreStep : public SoundPreStep
{
public:
    FlightWindAndPlasmaSoundPreStep(VesselXRSoundEngine *pEngine);
    virtual void clbkPreStep(const double simt, const double simdt, const double mjd) override;
};

class AutopilotOnOffSoundPreStep : public SoundPreStep
{
public:
    AutopilotOnOffSoundPreStep(VesselXRSoundEngine *pEngine);
    virtual void clbkPreStep(const double simt, const double simdt, const double mjd) override;

protected:
    int m_prevNavmode; // -1 = not set, 0 = OFF, 1-7 = nav mode
};

class DockingRadarDefaultSoundPreStep : public DefaultSoundPreStep
{
public:
    DockingRadarDefaultSoundPreStep(VesselXRSoundEngine *pEngine);
    virtual void clbkPreStep(const double simt, const double simdt, const double mjd) override;

protected:
    double m_realtimeNextBeep;  // -1 = no beep set
};

class DisableAutopilotsForTimeAccPreStep : public SoundPreStep
{
public:
    DisableAutopilotsForTimeAccPreStep(VesselXRSoundEngine *pEngine);
    virtual void clbkPreStep(const double simt, const double simdt, const double mjd) override;
};
