// ==============================================================
// Base class for the XRSound API engine implementation; this file is not distributed with XRSound.
// The code for this exists in XRSound.dll.
// 
// Copyright (c) 2018-2021 Douglas Beachy
// Licensed under the MIT License
// ==============================================================

#pragma once

#include <atlstr.h>             // for CString
#include <unordered_map>

#include "OrbiterSDK.h"
#include "XRSoundEngine30.h"   // latest interface version 

using namespace std;

class XRSoundConfigFileParser;
class VesselXRSoundEngine;
class ModuleXRSoundEngine;

// Redefine oapiGetSimTime as a compile-time error to prevent XRSound code from accidentally invoking it
// Note: ideally I would like to have this invoke '#error foo' to generate an elegant message, but there's no way to declare that
#define oapiGetSimTime ERROR! "Do not invoke oapiGetSimTime: see comment block in XRSoundDLL::clbkPreStep for details"

class ISound;
class ISoundEngine;
#ifdef XRSOUND_DLL_BUILD
#include "XRSoundConfigFileParser.h"
#else
// we're compiling XRSoundLib, so all we need is a forward reference (and we don't want to include the full class definition on the .lib side!)
class XRSoundConfigFileParser;
#endif

// Data object that contains all the state necessary to play back a single sound file (sound effect).
// This maintains state for a given sound across multiple frames. It persists until the vessel is destroyed
// or the vessel loads another wav file using the same sound ID (i.e., "sound slot") as this context uses.
struct WavContext
{
    // Normal constructor
    WavContext(const int soundID, const char *pSoundFilename, const XRSound::PlaybackType playbackType, const bool bEnabled) :
        soundID(soundID), csSoundFilename(pSoundFilename), playbackType(playbackType), 
        bPaused(false), bEnabled(bEnabled)
    {
        ResetPlaybackFields();  // initializes any fields not already initialized above
    }

    // Copy constructor
    WavContext(const WavContext &that)
    {
        soundID = that.soundID;
        csSoundFilename = that.csSoundFilename;
        playbackType = that.playbackType;
        bLoop = that.bLoop;
        volume = that.volume;
        pISound = that.pISound;
        bPaused = that.bPaused;
        bEnabled = that.bEnabled;
    }

    // reset all playback-related fields to their initial state, indicating the context is not (or not longer) in use
    void ResetPlaybackFields()
    {
        bLoop = false;
        volume = -1;        // "not set yet"
        pISound = nullptr;  // we will allocate a new ISound interface when (and if) the sound is played again
    }

    // used for logging
    CString ToStr(const bool bShowLoopAndVolume = true) const
    { 
        CString msg;
        if (bShowLoopAndVolume)
        {
            msg.Format("[soundID = %d, playbackType = %s (%d), bLoop = %d, volume = %.2lf, bPaused = %d, bEnabled = %d, filename = '%s']",
                soundID, PlaybackTypeToStr(playbackType), playbackType, bLoop, volume, bPaused, bEnabled, static_cast<const char *>(csSoundFilename));
        }
        else
        {
            msg.Format("[soundID = %d, playbackType = %s (%d), filename = '%s']",
                soundID, PlaybackTypeToStr(playbackType), playbackType, static_cast<const char *>(csSoundFilename));
        }
        
        return msg;
    }

    // Used for logging
    static const char* PlaybackTypeToStr(const XRSound::PlaybackType state)
    {
        switch (state) {
        case XRSound::PlaybackType::InternalOnly:
            return "InternalOnly";
        case XRSound::PlaybackType::BothViewFar:
            return "BothViewFar";
        case XRSound::PlaybackType::BothViewMedium:
            return "BothViewMedium";
        case XRSound::PlaybackType::BothViewClose:
            return "BothViewClose";
        case XRSound::PlaybackType::Radio:
            return "Radio";
        case XRSound::PlaybackType::Wind:
            return "Wind";
        case XRSound::PlaybackType::Global:
            return "Global";
        default:
            return "ERROR (unknown type)";
        }
    }

    // data added via LoadWav
    int soundID;
    CString csSoundFilename;
    XRSound::PlaybackType playbackType;   // InternalOnly, BothViewFar, Radio, etc.

    // data added via PlayWav and SetPaused
    bool bLoop; 
    float volume;       // 0..1.0; note that this always contains the *original requested volume*, never and *adjusted volume*
    bool bPaused;
    bool bEnabled;      // NOTE this is *only* used to enable or disable default sounds, NEVER for other sounds
    ISound *pISound;    // used to control and retrieve the state of the sound effect
};

// Abstract base class for all XRSoundEngine objects; Implements the XRSoundEngine 2.0/3.0 interface.
//
// WARNING: if using static linking, do not allocate or free any memory in any inline methods here: XRSound.dll uses its own heap, so any
// code that allocates or frees memory must reside in XRSound.dll, NOT in the calling DLL via non-virtual inline code
// in this header file.

// NOTE: these methods must all be *virtual* so that they can be located via the object pointer from inside 
// the caller's DLL: by design, there is no import library for XRSound.dll.
// Do NOT use any inline code for any methods invoked by XRSoundImpl, or it will reside in the caller's DLL instead of XRSound.dll.
//
// NOTE: this class is not thread-safe!  However, Orbiter is not multi-threaded, so that should not be an issue.
class XRSoundEngine : public XRSoundEngine30   
{
public:
    // Note: constructor and destructor are private by design; callers should always use CreateInstance (defined in each subclass) 
    // and DestroyInstance to create and destroy instances of XRSoundEngine.
    static void DestroyInstance(XRSoundEngine *pInst);

    //============================================================================================
    // Public abstract methods: these implement the XRSoundEngine interfaces
    //============================================================================================
    // XRSound version 1.x 
    virtual float GetVersion() const override;
    virtual bool LoadWav(const int soundID, const char *pSoundFilename, const XRSound::PlaybackType playbackType) override;
    virtual bool PlayWav(const int soundID, const bool bLoop = false, const float volume = 1.0) override;
    virtual bool StopWav(const int soundID) override;
    virtual bool IsWavPlaying(const int soundID) override;
    virtual bool SetPaused(const int soundID, const bool bPause) override;
    virtual bool IsPaused(const int soundID) override;

    virtual bool SetDefaultSoundEnabled(const XRSound::DefaultSoundID soundID, const bool bEnabled) override = 0;
    virtual bool GetDefaultSoundEnabled(const XRSound::DefaultSoundID soundID) override = 0;
    
    virtual bool SetDefaultSoundGroupFolder(const XRSound::DefaultSoundID groupSoundID, const char *pSubfolderPath) override = 0;
    virtual const char *GetDefaultSoundGroupFolder(const XRSound::DefaultSoundID groupSoundID) const override = 0;

    // these are not part of the public XRSound API, but are exposed via the XRSoundEngine interface so the XRSoundImpl side can call them
    virtual bool IsDefaultSound(const int soundID) const override;
    virtual bool IsDefaultSoundGroup(const int soundID) const override;
    virtual bool IsDefaultSoundOrGroup(const int soundID) const override;

protected:
    // these are protected to prevent incorrect creation or deletion outside of our subclasses' static CreateInstance methods and our DestroyInstance method
    XRSoundEngine();
    virtual ~XRSoundEngine();

public:
    // XRSound version 2.x 
    virtual EngineType GetEngineType() override = 0;
    virtual const char *GetLogID() override = 0;  // e.g., vessel or module name

    // XRSound version 3.x
    virtual bool  SetPan(const int soundID, const float pan);
    virtual float GetPan(const int soundID);

    virtual bool  SetPlaybackSpeed(const int soundID, const float speed = 1.0);
    virtual float GetPlaybackSpeed(const int soundID);

    virtual bool  SetPlayPosition(const int soundID, const unsigned int positionMillis);
    virtual int   GetPlayPosition(const int soundID);
    
    // --------------------------------------------------------------------

    // Methods only invoked by XRSoundDLL (our Orbiter module class).  Because these are non-virtual, it is impossible for XRSoundLib to invoke them
    // because it cannot link with them when the Orbiter vessel using XRSoundLib tries to link.
    static void DestroySoundEngine();     // performs one-time destruction of our sound engine
    static bool IsSoundEngineInitialized();
    static const char *GetSoundDriverName();
    static const char *GetVersionStr();
    static void ResetStaticSimulationData();
    static float ComputeVariableVolume(const double minVolume, const double maxVolume, double level);
    static double ComputeVariableLevel(const double min, const double max, double level);

    //=====================================================================
    // non-static methods (which are therefore not callable by XRSoundLib)
    //=====================================================================
    void WriteLog(const char *pMsg);   // can't be const b/c we need to call oapiGetVesselInterface
    void StopAllWav();     
    void SetAllWavPaused(const bool bPaused);     
    
    XRSoundConfigFileParser &GetConfig() { _ASSERTE(m_pConfig);  return *m_pConfig; }
    vector<CString> GetValidSoundFileExtensions();    // e.g., ".ogg", ".wav", ".mp3", etc.
    const char *GetWavFilename(const int soundID);
    
    // returns Orbiter's camera's global coordinates
    static VECTOR3 GetCameraCoordinates()
    {
        VECTOR3 cameraGlobalCoords;
        oapiCameraGlobalPos(&cameraGlobalCoords);
        return cameraGlobalCoords;
    }

    static bool PauseOrReumseMusic(const bool bPause);

    // static data
    static XRSoundConfigFileParser s_globalConfig;    // this is parsed at DLL initialization time so we can write to the log file and read configuration data early-on from our static methods

protected:
    static bool InitializeSoundEngine();  // performs one-time static initialization of our sound singleton

    //=====================================================================================
    // Protected abstract methods -- these are not exposed via the XRSoundEngine interface.
    //=====================================================================================
    virtual void FreeResources() = 0;  // NOTE: *always* use this method to release XRSoundEngine resources before deleting it; invoked from our static DestroyInstance method.
    virtual void UpdateSoundState(WavContext &context) = 0;

    bool VerboseLogging() const;
    WavContext *FindWavContext(const int soundID);
    static bool StopWavImpl(WavContext *pContext, XRSoundEngine *pEngine);
    
    // data
    static ISoundEngine *s_SoundEngine;        // initialized by InitializeSoundEngine
    static bool s_bSoundEngineNeedsInitialization; // used to handle one-time startup items

    XRSoundConfigFileParser *m_pConfig;   // this is per-engine instance instead of static so that we can per-vessel or per-module configuration overrides if we want to    
    
    // Defines the master map of all sounds known by this engine instance (i.e., known to this vessel or module).
    // key = soundID, value = WavContext for that soundID
    typedef unordered_map<int, WavContext> HASHMAP_SOUNDID_WAVCONTEXT;
    typedef pair<int, WavContext &> soundID_WavContext_Pair;

    // map of all sound ID -> wav context pairs known by this vessel
    HASHMAP_SOUNDID_WAVCONTEXT m_allWavsMap;

private:
    static WavContext *s_pMusicFolderWavContext;  // our special, global MusicFolder sound
    static CString s_csVersion;
    CString m_lastLogLine;  // last log message written to XRSound.log
    double m_nextDuplicateLogLineSimt;
};

// this macro is designed to be use with an XRSoundEngine instance (reference)
#define VERBOSE_LOG(pEngine, str, ...)              \
{                                                   \
    if (pEngine->GetConfig().EnableVerboseLogging)  \
    {                                               \
        CString msg; msg.Format(str, __VA_ARGS__);  \
        pEngine->WriteLog(msg);                     \
    }                                               \
}

// utility macros
#define MPS_TO_KNOTS(mps)   (mps / 0.5148)    /* meters-per-second to knots */
#define KNOTS_TO_MPS(knots) (knots * 0.5148)  /* knots to meters-per-second */

// These are used to dynamically bind to DLL-exported methods.  Note that DLLCLBK specifies 'extern "C"', which uses the __cdecl
// calling convention, NOT the normal __stdcall that C++ uses.
extern "C" typedef XRSoundEngine* (__cdecl* VesselXRSoundEngineInstanceFuncPtr)(OBJHANDLE hVessel);
extern "C" typedef XRSoundEngine* (__cdecl* ModuleXRSoundEngineInstanceFuncPtr)(const char* pUniqueModuleName);
